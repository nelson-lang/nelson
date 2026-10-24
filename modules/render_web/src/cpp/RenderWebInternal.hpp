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
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
uint64_t
renderWebRgbaContentHash(const unsigned char* rgba, int width, int height, int stride);
//=============================================================================
bool
renderWebRgbaToPngBase64(
    const unsigned char* rgba, int width, int height, int stride, std::string& base64);
//=============================================================================
bool
renderWebCachedRgbaToPngBase64(const unsigned char* rgba, int width, int height, int stride,
    std::string& base64, std::string& token, bool& includePayload);
//=============================================================================
bool
renderWebRawRgbaPayload(const unsigned char* rgba, int width, int height, int stride,
    bool contentToken, std::vector<unsigned char>& payload, std::string& token,
    bool& includePayload, bool alwaysIncludePayload = false);
//=============================================================================
} // namespace Nelson
//=============================================================================
