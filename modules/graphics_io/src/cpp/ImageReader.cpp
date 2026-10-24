//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ImageReader.hpp"
#include "RasterCodec.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    ArrayOf
    emptyMatrix()
    {
        return ArrayOf::empty(0, 0);
    }
    //=============================================================================
    ArrayOf
    grayImage(const RasterImage& image)
    {
        Dimensions dimensions(image.height, image.width);
        if (image.format == RasterPixelFormat::Gray16) {
            auto* output = static_cast<uint16*>(
                ArrayOf::allocateArrayOf(NLS_UINT16, dimensions.elementCount()));
            const auto* input = reinterpret_cast<const uint16_t*>(image.pixels.data());
            for (int y = 0; y < image.height; ++y) {
                for (int x = 0; x < image.width; ++x) {
                    output[y + static_cast<size_t>(x) * image.height]
                        = input[static_cast<size_t>(y) * image.width + x];
                }
            }
            return ArrayOf(NLS_UINT16, dimensions, output);
        }
        auto* output
            = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, dimensions.elementCount()));
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                output[y + static_cast<size_t>(x) * image.height]
                    = image.pixels[static_cast<size_t>(y) * image.width + x];
            }
        }
        return ArrayOf(NLS_UINT8, dimensions, output);
    }
    //=============================================================================
    ArrayOf
    colorImage(const RasterImage& image)
    {
        Dimensions dimensions(std::vector<indexType> {
            static_cast<indexType>(image.height), static_cast<indexType>(image.width), 3 });
        auto* output
            = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, dimensions.elementCount()));
        const size_t slice = static_cast<size_t>(image.width) * image.height;
        const int channels = image.channels();
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                const size_t source = (static_cast<size_t>(y) * image.width + x) * channels;
                const size_t target = y + static_cast<size_t>(x) * image.height;
                output[target] = image.pixels[source];
                output[target + slice] = image.pixels[source + 1];
                output[target + slice * 2] = image.pixels[source + 2];
            }
        }
        return ArrayOf(NLS_UINT8, dimensions, output);
    }
    //=============================================================================
    ArrayOf
    indexedImage(const RasterImage& image)
    {
        Dimensions dimensions(image.height, image.width);
        auto* output
            = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, dimensions.elementCount()));
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                output[y + static_cast<size_t>(x) * image.height]
                    = image.pixels[static_cast<size_t>(y) * image.width + x];
            }
        }
        return ArrayOf(NLS_UINT8, dimensions, output);
    }
    //=============================================================================
    ArrayOf
    colorMap(const RasterImage& image)
    {
        const indexType count = static_cast<indexType>(image.palette.size() / 4);
        Dimensions dimensions(count, 3);
        auto* output
            = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, dimensions.elementCount()));
        for (indexType i = 0; i < count; ++i) {
            output[i] = image.palette[static_cast<size_t>(i) * 4] / 255.0;
            output[i + count] = image.palette[static_cast<size_t>(i) * 4 + 1] / 255.0;
            output[i + count * 2] = image.palette[static_cast<size_t>(i) * 4 + 2] / 255.0;
        }
        return ArrayOf(NLS_DOUBLE, dimensions, output);
    }
    //=============================================================================
    ArrayOf
    alphaImage(const RasterImage& image)
    {
        if (image.format != RasterPixelFormat::RGBA8
            && image.format != RasterPixelFormat::Indexed8) {
            return emptyMatrix();
        }
        Dimensions dimensions(image.height, image.width);
        auto* output
            = static_cast<uint8*>(ArrayOf::allocateArrayOf(NLS_UINT8, dimensions.elementCount()));
        for (int y = 0; y < image.height; ++y) {
            for (int x = 0; x < image.width; ++x) {
                const size_t pixel = static_cast<size_t>(y) * image.width + x;
                uint8_t alpha = 255;
                if (image.format == RasterPixelFormat::RGBA8) {
                    alpha = image.pixels[pixel * 4 + 3];
                }
                if (image.format == RasterPixelFormat::Indexed8) {
                    const size_t paletteOffset = static_cast<size_t>(image.pixels[pixel]) * 4 + 3;
                    if (paletteOffset < image.palette.size()) {
                        alpha = image.palette[paletteOffset];
                    }
                }
                output[y + static_cast<size_t>(x) * image.height] = alpha;
            }
        }
        return ArrayOf(NLS_UINT8, dimensions, output);
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
imageReader(const std::wstring& filename, int nLhs)
{
    RasterImage image;
    std::string error;
    if (!readRasterImage(filename, image, error)) {
        Error(L"Nelson:imread:cannotReadFile", L"Impossible read image file: %s",
            utf8_to_wstring(error));
    }
    ArrayOfVector results;
    if (image.format == RasterPixelFormat::Indexed8) {
        results << indexedImage(image);
    } else if (image.format == RasterPixelFormat::Gray8
        || image.format == RasterPixelFormat::Gray16) {
        results << grayImage(image);
    } else {
        results << colorImage(image);
    }
    if (nLhs > 1) {
        results << (image.format == RasterPixelFormat::Indexed8 ? colorMap(image) : emptyMatrix());
    }
    if (nLhs > 2) {
        results << alphaImage(image);
    }
    return results;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
