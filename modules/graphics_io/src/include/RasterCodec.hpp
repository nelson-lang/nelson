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
#include <string>
#include <vector>
#include "RasterImage.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct RasterFormatInfo
{
    std::string extension;
    std::vector<std::string> aliases;
    std::string description;
    bool readable;
    bool writable;
    bool alpha;
    bool multipage;
};
//=============================================================================
const std::vector<RasterFormatInfo>&
rasterFormats();
const RasterFormatInfo*
rasterFormat(const std::string& format);
std::string
normalizeRasterFormat(const std::string& format);
std::string
detectRasterFormat(const std::vector<uint8_t>& bytes, const std::string& extensionHint = {});
bool
readRasterImage(const std::wstring& filename, RasterImage& image, std::string& error);
bool
writeRasterImage(const std::wstring& filename, const std::string& format, const RasterImage& image,
    int quality, std::string& error);
bool
writeRasterImageAtomic(const std::wstring& filename, const std::string& format,
    const RasterImage& image, int quality, std::string& error);
bool
encodeRasterImage(const std::string& format, const RasterImage& image, int quality,
    std::vector<uint8_t>& bytes, std::string& error);
bool
decodeRasterImage(const std::vector<uint8_t>& bytes, const std::string& extensionHint,
    RasterImage& image, std::string& error);
bool
writeRasterGif(const std::wstring& filename, const RasterImage& image, bool append, int delayMs,
    int loopCount, std::string& error);
//=============================================================================
} // namespace Nelson
//=============================================================================
