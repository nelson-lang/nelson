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
#include <cstring>
//=============================================================================
#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
#define STBI_WRITE_NO_STDIO
#define STB_IMAGE_WRITE_STATIC
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
decodeStb(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    int width = 0;
    int height = 0;
    int sourceChannels = 0;
    unsigned char* pixels = stbi_load_from_memory(
        bytes.data(), static_cast<int>(bytes.size()), &width, &height, &sourceChannels, 0);
    if (pixels == nullptr) {
        error = stbi_failure_reason() ? stbi_failure_reason() : "unsupported image data";
        return false;
    }
    const int channels
        = sourceChannels == 1 ? 1 : (sourceChannels == 2 || sourceChannels == 4 ? 4 : 3);
    image.width = width;
    image.height = height;
    image.format = channels == 1
        ? RasterPixelFormat::Gray8
        : (channels == 4 ? RasterPixelFormat::RGBA8 : RasterPixelFormat::RGB8);
    const size_t count = static_cast<size_t>(width) * height;
    image.pixels.resize(count * static_cast<size_t>(channels));
    if (sourceChannels == 2) {
        for (size_t i = 0; i < count; ++i) {
            image.pixels[i * 4] = image.pixels[i * 4 + 1] = image.pixels[i * 4 + 2] = pixels[i * 2];
            image.pixels[i * 4 + 3] = pixels[i * 2 + 1];
        }
    } else {
        std::memcpy(image.pixels.data(), pixels, image.pixels.size());
    }
    stbi_image_free(pixels);
    return true;
}
//=============================================================================
static void
appendBytes(void* context, void* data, int size)
{
    auto* output = static_cast<std::vector<uint8_t>*>(context);
    auto* bytes = static_cast<uint8_t*>(data);
    output->insert(output->end(), bytes, bytes + size);
}
//=============================================================================
bool
encodeStb(const std::string& format, const RasterImage& image, int quality,
    std::vector<uint8_t>& bytes, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images are only writable as PNG, TIFF or PGM";
        return false;
    }
    const bool keepAlpha = format == "png" || format == "bmp" || format == "tga";
    const int channels = keepAlpha && image.format == RasterPixelFormat::RGBA8 ? 4 : 3;
    std::vector<uint8_t> pixels = expandedRasterPixels(image, channels);
    int result = 0;
    if (format == "png") {
        result = stbi_write_png_to_func(appendBytes, &bytes, image.width, image.height, channels,
            pixels.data(), image.width * channels);
    } else if (format == "jpg") {
        result = stbi_write_jpg_to_func(
            appendBytes, &bytes, image.width, image.height, channels, pixels.data(), quality);
    } else if (format == "bmp") {
        result = stbi_write_bmp_to_func(
            appendBytes, &bytes, image.width, image.height, channels, pixels.data());
    } else if (format == "tga") {
        result = stbi_write_tga_to_func(
            appendBytes, &bytes, image.width, image.height, channels, pixels.data());
    }
    if (!result) {
        error = "image encoder failed";
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
