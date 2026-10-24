//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cstdint>
#include <map>
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class RasterPixelFormat
{
    Indexed8,
    Gray8,
    Gray16,
    RGB8,
    RGBA8
};
//=============================================================================
struct RasterImage
{
    int width { 0 };
    int height { 0 };
    RasterPixelFormat format { RasterPixelFormat::RGB8 };
    std::vector<uint8_t> pixels;
    std::vector<uint8_t> palette;
    std::map<std::string, std::string> metadata;

    bool
    valid() const;
    int
    channels() const;
    int
    bytesPerChannel() const;
    size_t
    rowBytes() const;
};
//=============================================================================
void
rasterPixelRgba(const RasterImage& image, size_t index, uint8_t rgba[4]);
std::vector<uint8_t>
expandedRasterPixels(const RasterImage& image, int channels);
//=============================================================================
} // namespace Nelson
//=============================================================================
