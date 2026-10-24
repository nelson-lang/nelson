//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterCodecInternal.hpp"
#include <algorithm>
#include <cstring>
#include <png.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    struct PngReadContext
    {
        const std::vector<uint8_t>& bytes;
        size_t offset { 0 };
    };
    //=============================================================================
    struct PngWriteContext
    {
        std::vector<uint8_t>& bytes;
    };
    //=============================================================================
    void
    readPngBytes(png_structp png, png_bytep destination, png_size_t count)
    {
        auto* context = static_cast<PngReadContext*>(png_get_io_ptr(png));
        if (context->offset + count > context->bytes.size()) {
            png_error(png, "truncated PNG");
        }
        std::memcpy(destination, context->bytes.data() + context->offset, count);
        context->offset += count;
    }
    //=============================================================================
    void
    writePngBytes(png_structp png, png_bytep source, png_size_t count)
    {
        auto* context = static_cast<PngWriteContext*>(png_get_io_ptr(png));
        context->bytes.insert(context->bytes.end(), source, source + count);
    }
    //=============================================================================
    void
    flushPng(png_structp)
    {
    }
    //=============================================================================
    bool
    readPalette(png_structp png, png_infop info, RasterImage& image)
    {
        png_colorp colors = nullptr;
        int colorCount = 0;
        if (!png_get_PLTE(png, info, &colors, &colorCount) || colorCount <= 0) {
            return false;
        }
        image.palette.assign(static_cast<size_t>(colorCount) * 4, 255);
        for (int i = 0; i < colorCount; ++i) {
            image.palette[static_cast<size_t>(i) * 4] = colors[i].red;
            image.palette[static_cast<size_t>(i) * 4 + 1] = colors[i].green;
            image.palette[static_cast<size_t>(i) * 4 + 2] = colors[i].blue;
        }
        png_bytep alpha = nullptr;
        int alphaCount = 0;
        png_color_16p transparent = nullptr;
        if (png_get_tRNS(png, info, &alpha, &alphaCount, &transparent)) {
            for (int i = 0; i < std::min(alphaCount, colorCount); ++i) {
                image.palette[static_cast<size_t>(i) * 4 + 3] = alpha[i];
            }
        }
        return true;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodePng(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    png_infop info = png ? png_create_info_struct(png) : nullptr;
    if (!png || !info) {
        if (png) {
            png_destroy_read_struct(&png, nullptr, nullptr);
        }
        error = "cannot initialize PNG decoder";
        return false;
    }
    if (setjmp(png_jmpbuf(png))) {
        png_destroy_read_struct(&png, &info, nullptr);
        error = "invalid PNG image";
        return false;
    }
    PngReadContext context { bytes };
    png_set_read_fn(png, &context, readPngBytes);
    png_read_info(png, info);
    image.width = static_cast<int>(png_get_image_width(png, info));
    image.height = static_cast<int>(png_get_image_height(png, info));
    const int colorType = png_get_color_type(png, info);
    const int bitDepth = png_get_bit_depth(png, info);
    const bool hasTransparency = png_get_valid(png, info, PNG_INFO_tRNS) != 0;
    if (colorType == PNG_COLOR_TYPE_PALETTE) {
        if (bitDepth < 8) {
            png_set_packing(png);
        }
        image.format = RasterPixelFormat::Indexed8;
        readPalette(png, info, image);
    } else if (colorType == PNG_COLOR_TYPE_GRAY && hasTransparency) {
        if (bitDepth == 16) {
            png_set_strip_16(png);
        } else if (bitDepth < 8) {
            png_set_expand_gray_1_2_4_to_8(png);
        }
        png_set_gray_to_rgb(png);
        png_set_tRNS_to_alpha(png);
        image.format = RasterPixelFormat::RGBA8;
    } else if (colorType == PNG_COLOR_TYPE_GRAY && bitDepth == 16) {
        image.format = RasterPixelFormat::Gray16;
#if PNG_LIBPNG_VER >= 10504
        png_set_swap(png);
#endif
    } else if (colorType == PNG_COLOR_TYPE_GRAY) {
        if (bitDepth < 8) {
            png_set_expand_gray_1_2_4_to_8(png);
        }
        image.format = RasterPixelFormat::Gray8;
    } else {
        if (bitDepth == 16) {
            png_set_strip_16(png);
        }
        if (colorType == PNG_COLOR_TYPE_GRAY_ALPHA) {
            png_set_gray_to_rgb(png);
        }
        if (hasTransparency) {
            png_set_tRNS_to_alpha(png);
        }
        image.format = (colorType & PNG_COLOR_MASK_ALPHA) || hasTransparency
            ? RasterPixelFormat::RGBA8
            : RasterPixelFormat::RGB8;
    }
    png_read_update_info(png, info);
    image.pixels.resize(png_get_rowbytes(png, info) * static_cast<size_t>(image.height));
    std::vector<png_bytep> rows(static_cast<size_t>(image.height));
    for (int row = 0; row < image.height; ++row) {
        rows[static_cast<size_t>(row)]
            = image.pixels.data() + static_cast<size_t>(row) * image.rowBytes();
    }
    png_read_image(png, rows.data());
    png_read_end(png, nullptr);
    png_destroy_read_struct(&png, &info, nullptr);
    if (!image.valid()) {
        error = "unsupported PNG layout";
        return false;
    }
    return true;
}
//=============================================================================
bool
encodePng(const RasterImage& image, std::vector<uint8_t>& bytes, std::string& error)
{
    png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
    png_infop info = png ? png_create_info_struct(png) : nullptr;
    if (!png || !info) {
        if (png) {
            png_destroy_write_struct(&png, nullptr);
        }
        error = "cannot initialize PNG encoder";
        return false;
    }
    if (setjmp(png_jmpbuf(png))) {
        png_destroy_write_struct(&png, &info);
        error = "PNG encoder failed";
        return false;
    }
    PngWriteContext context { bytes };
    png_set_write_fn(png, &context, writePngBytes, flushPng);
    int colorType = PNG_COLOR_TYPE_RGB;
    int bitDepth = 8;
    if (image.format == RasterPixelFormat::Indexed8) {
        colorType = PNG_COLOR_TYPE_PALETTE;
    }
    if (image.format == RasterPixelFormat::Gray8) {
        colorType = PNG_COLOR_TYPE_GRAY;
    }
    if (image.format == RasterPixelFormat::Gray16) {
        colorType = PNG_COLOR_TYPE_GRAY;
        bitDepth = 16;
    }
    if (image.format == RasterPixelFormat::RGBA8) {
        colorType = PNG_COLOR_TYPE_RGBA;
    }
    png_set_IHDR(png, info, image.width, image.height, bitDepth, colorType, PNG_INTERLACE_NONE,
        PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
    std::vector<png_color> colors;
    std::vector<png_byte> alpha;
    if (image.format == RasterPixelFormat::Indexed8) {
        const size_t count = image.palette.size() / 4;
        colors.resize(count);
        alpha.resize(count);
        bool hasAlpha = false;
        for (size_t i = 0; i < count; ++i) {
            colors[i]
                = { image.palette[i * 4], image.palette[i * 4 + 1], image.palette[i * 4 + 2] };
            alpha[i] = image.palette[i * 4 + 3];
            hasAlpha = hasAlpha || alpha[i] != 255;
        }
        png_set_PLTE(png, info, colors.data(), static_cast<int>(colors.size()));
        if (hasAlpha) {
            png_set_tRNS(png, info, alpha.data(), static_cast<int>(alpha.size()), nullptr);
        }
    }
    std::vector<png_text> texts;
    texts.reserve(image.metadata.size());
    for (const auto& item : image.metadata) {
        if (item.first == "Comment" || item.first == "Author") {
            png_text text {};
            text.compression = PNG_TEXT_COMPRESSION_NONE;
            text.key = const_cast<char*>(item.first.c_str());
            text.text = const_cast<char*>(item.second.c_str());
            texts.push_back(text);
        }
    }
    if (!texts.empty()) {
        png_set_text(png, info, texts.data(), static_cast<int>(texts.size()));
    }
    png_write_info(png, info);
#if PNG_LIBPNG_VER >= 10504
    if (image.format == RasterPixelFormat::Gray16) {
        png_set_swap(png);
    }
#endif
    std::vector<png_bytep> rows(static_cast<size_t>(image.height));
    for (int row = 0; row < image.height; ++row) {
        rows[static_cast<size_t>(row)] = const_cast<png_bytep>(
            image.pixels.data() + static_cast<size_t>(row) * image.rowBytes());
    }
    png_write_image(png, rows.data());
    png_write_end(png, nullptr);
    png_destroy_write_struct(&png, &info);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
