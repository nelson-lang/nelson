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
#include <cstdlib>
#include <webp/decode.h>
#include <webp/encode.h>
//=============================================================================
namespace Nelson {
bool
decodeWebp(const std::vector<uint8_t>& bytes, RasterImage& image, std::string& error)
{
    WebPBitstreamFeatures features {};
    if (WebPGetFeatures(bytes.data(), bytes.size(), &features) != VP8_STATUS_OK) {
        error = "invalid WebP image";
        return false;
    }
    int width = features.width;
    int height = features.height;
    uint8_t* pixels = features.has_alpha
        ? WebPDecodeRGBA(bytes.data(), bytes.size(), &width, &height)
        : WebPDecodeRGB(bytes.data(), bytes.size(), &width, &height);
    if (pixels == nullptr) {
        error = "WebP decoder failed";
        return false;
    }
    image.width = width;
    image.height = height;
    image.format = features.has_alpha ? RasterPixelFormat::RGBA8 : RasterPixelFormat::RGB8;
    image.pixels.assign(
        pixels, pixels + static_cast<size_t>(width) * height * (features.has_alpha ? 4 : 3));
    WebPFree(pixels);
    return true;
}
//=============================================================================
bool
encodeWebp(const RasterImage& image, int quality, std::vector<uint8_t>& bytes, std::string& error)
{
    if (image.format == RasterPixelFormat::Gray16) {
        error = "16-bit images cannot be written as WebP";
        return false;
    }
    std::vector<uint8_t> rgba = expandedRasterPixels(image, 4);
    uint8_t* encoded = nullptr;
    const size_t size = WebPEncodeRGBA(rgba.data(), image.width, image.height, image.width * 4,
        static_cast<float>(quality), &encoded);
    if (size == 0 || encoded == nullptr) {
        error = "WebP encoder failed";
        return false;
    }
    bytes.assign(encoded, encoded + size);
    WebPFree(encoded);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
