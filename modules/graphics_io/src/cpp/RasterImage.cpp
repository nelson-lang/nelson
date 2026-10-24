//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "RasterImage.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
//=============================================================================
int
RasterImage::channels() const
{
    switch (format) {
    case RasterPixelFormat::Indexed8:
    case RasterPixelFormat::Gray8:
    case RasterPixelFormat::Gray16:
        return 1;
    case RasterPixelFormat::RGB8:
        return 3;
    case RasterPixelFormat::RGBA8:
        return 4;
    }
    return 0;
}
//=============================================================================
int
RasterImage::bytesPerChannel() const
{
    return format == RasterPixelFormat::Gray16 ? 2 : 1;
}
//=============================================================================
size_t
RasterImage::rowBytes() const
{
    return static_cast<size_t>(width) * channels() * bytesPerChannel();
}
//=============================================================================
bool
RasterImage::valid() const
{
    return width > 0 && height > 0 && pixels.size() == rowBytes() * static_cast<size_t>(height)
        && (format != RasterPixelFormat::Indexed8 || palette.size() % 4 == 0);
}
//=============================================================================
void
rasterPixelRgba(const RasterImage& image, size_t index, uint8_t rgba[4])
{
    rgba[0] = rgba[1] = rgba[2] = 0;
    rgba[3] = 255;
    if (image.format == RasterPixelFormat::Indexed8) {
        const size_t offset = static_cast<size_t>(image.pixels[index]) * 4;
        if (offset + 3 < image.palette.size()) {
            std::copy_n(image.palette.data() + offset, 4, rgba);
        }
    } else if (image.format == RasterPixelFormat::Gray8) {
        rgba[0] = rgba[1] = rgba[2] = image.pixels[index];
    } else if (image.format == RasterPixelFormat::RGB8) {
        std::copy_n(image.pixels.data() + index * 3, 3, rgba);
    } else if (image.format == RasterPixelFormat::RGBA8) {
        std::copy_n(image.pixels.data() + index * 4, 4, rgba);
    }
}
//=============================================================================
std::vector<uint8_t>
expandedRasterPixels(const RasterImage& image, int channels)
{
    const size_t count = static_cast<size_t>(image.width) * image.height;
    std::vector<uint8_t> output(count * static_cast<size_t>(channels));
    for (size_t i = 0; i < count; ++i) {
        uint8_t rgba[4];
        rasterPixelRgba(image, i, rgba);
        std::copy_n(rgba, channels, output.data() + i * static_cast<size_t>(channels));
    }
    return output;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
