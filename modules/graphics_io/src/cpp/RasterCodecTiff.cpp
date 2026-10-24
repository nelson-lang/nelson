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
#include <filesystem>
#include <tiffio.h>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    TIFF*
    openTiff(const std::wstring& filename, const char* mode)
    {
#ifdef _WIN32
        return TIFFOpenW(filename.c_str(), mode);
#else
        return TIFFOpen(std::filesystem::path(filename).string().c_str(), mode);
#endif
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
decodeTiff(const std::wstring& filename, RasterImage& image, std::string& error)
{
    TIFF* tiff = openTiff(filename, "r");
    if (!tiff) {
        error = "cannot open TIFF image";
        return false;
    }
    uint32_t width = 0;
    uint32_t height = 0;
    uint16_t bits = 8;
    uint16_t samples = 1;
    uint16_t sampleFormat = SAMPLEFORMAT_UINT;
    TIFFGetField(tiff, TIFFTAG_IMAGEWIDTH, &width);
    TIFFGetField(tiff, TIFFTAG_IMAGELENGTH, &height);
    TIFFGetFieldDefaulted(tiff, TIFFTAG_BITSPERSAMPLE, &bits);
    TIFFGetFieldDefaulted(tiff, TIFFTAG_SAMPLESPERPIXEL, &samples);
    TIFFGetFieldDefaulted(tiff, TIFFTAG_SAMPLEFORMAT, &sampleFormat);
    uint16_t extraCount = 0;
    uint16_t* extraTypes = nullptr;
    const bool hasExplicitAlpha
        = TIFFGetField(tiff, TIFFTAG_EXTRASAMPLES, &extraCount, &extraTypes) != 0 && extraCount > 0
        && std::any_of(extraTypes, extraTypes + extraCount, [](uint16_t type) {
               return type == EXTRASAMPLE_ASSOCALPHA || type == EXTRASAMPLE_UNASSALPHA;
           });
    image.width = static_cast<int>(width);
    image.height = static_cast<int>(height);
    if (bits == 16 && samples == 1 && sampleFormat == SAMPLEFORMAT_UINT) {
        image.format = RasterPixelFormat::Gray16;
        image.pixels.resize(static_cast<size_t>(width) * height * 2);
        const tmsize_t scanlineSize = TIFFScanlineSize(tiff);
        std::vector<uint8_t> row(static_cast<size_t>(scanlineSize));
        for (uint32_t y = 0; y < height; ++y) {
            if (TIFFReadScanline(tiff, row.data(), y, 0) < 0) {
                TIFFClose(tiff);
                error = "cannot decode TIFF scanline";
                return false;
            }
            std::copy_n(row.data(), static_cast<size_t>(width) * 2,
                image.pixels.data() + static_cast<size_t>(y) * width * 2);
        }
    } else {
        std::vector<uint32_t> rgba(static_cast<size_t>(width) * height);
        if (!TIFFReadRGBAImageOriented(tiff, width, height, rgba.data(), ORIENTATION_TOPLEFT, 0)) {
            TIFFClose(tiff);
            error = "cannot decode TIFF image";
            return false;
        }
        bool hasAlpha = hasExplicitAlpha;
        for (uint32_t pixel : rgba) {
            hasAlpha = hasAlpha || TIFFGetA(pixel) != 255;
        }
        image.format = hasAlpha ? RasterPixelFormat::RGBA8 : RasterPixelFormat::RGB8;
        image.pixels.resize(static_cast<size_t>(width) * height * image.channels());
        for (size_t i = 0; i < rgba.size(); ++i) {
            image.pixels[i * image.channels()] = TIFFGetR(rgba[i]);
            image.pixels[i * image.channels() + 1] = TIFFGetG(rgba[i]);
            image.pixels[i * image.channels() + 2] = TIFFGetB(rgba[i]);
            if (hasAlpha) {
                image.pixels[i * 4 + 3] = TIFFGetA(rgba[i]);
            }
        }
    }
    TIFFClose(tiff);
    return image.valid();
}
//=============================================================================
bool
encodeTiff(const std::wstring& filename, const RasterImage& image, std::string& error)
{
    TIFF* tiff = openTiff(filename, "w");
    if (!tiff) {
        error = "cannot create TIFF image";
        return false;
    }
    const bool gray16 = image.format == RasterPixelFormat::Gray16;
    const bool alpha = image.format == RasterPixelFormat::RGBA8
        || (image.format == RasterPixelFormat::Indexed8
            && std::any_of(image.palette.begin() + std::min<size_t>(3, image.palette.size()),
                image.palette.end(), [&image, index = size_t { 3 }](uint8_t value) mutable {
                    const bool result = index % 4 == 3 && value != 255;
                    ++index;
                    return result;
                }));
    const uint16_t samples = gray16 ? 1 : (alpha ? 4 : 3);
    TIFFSetField(tiff, TIFFTAG_IMAGEWIDTH, image.width);
    TIFFSetField(tiff, TIFFTAG_IMAGELENGTH, image.height);
    TIFFSetField(tiff, TIFFTAG_SAMPLESPERPIXEL, samples);
    TIFFSetField(tiff, TIFFTAG_BITSPERSAMPLE, gray16 ? 16 : 8);
    TIFFSetField(tiff, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
    TIFFSetField(tiff, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
    TIFFSetField(tiff, TIFFTAG_PHOTOMETRIC, gray16 ? PHOTOMETRIC_MINISBLACK : PHOTOMETRIC_RGB);
    TIFFSetField(tiff, TIFFTAG_COMPRESSION, COMPRESSION_LZW);
    auto comment = image.metadata.find("Comment");
    if (comment != image.metadata.end()) {
        TIFFSetField(tiff, TIFFTAG_IMAGEDESCRIPTION, comment->second.c_str());
    }
    auto author = image.metadata.find("Author");
    if (author != image.metadata.end()) {
        TIFFSetField(tiff, TIFFTAG_ARTIST, author->second.c_str());
    }
    if (alpha) {
        uint16_t extra = EXTRASAMPLE_UNASSALPHA;
        TIFFSetField(tiff, TIFFTAG_EXTRASAMPLES, 1, &extra);
    }
    std::vector<uint8_t> row(static_cast<size_t>(image.width) * samples * (gray16 ? 2 : 1));
    for (int y = 0; y < image.height; ++y) {
        if (gray16) {
            std::copy_n(image.pixels.data() + static_cast<size_t>(y) * image.width * 2,
                static_cast<size_t>(image.width) * 2, row.data());
        } else {
            for (int x = 0; x < image.width; ++x) {
                uint8_t rgba[4] = {};
                rasterPixelRgba(image, static_cast<size_t>(y) * image.width + x, rgba);
                std::copy_n(rgba, samples, row.data() + static_cast<size_t>(x) * samples);
            }
        }
        if (TIFFWriteScanline(tiff, row.data(), y, 0) < 0) {
            TIFFClose(tiff);
            error = "cannot encode TIFF scanline";
            return false;
        }
    }
    TIFFClose(tiff);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
