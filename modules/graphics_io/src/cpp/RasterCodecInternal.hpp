//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "RasterCodec.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
decodePng(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodePng(const RasterImage&, std::vector<uint8_t>&, std::string&);
bool
decodeJpeg(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodeJpeg(const RasterImage&, int, std::vector<uint8_t>&, std::string&);
bool
decodeWebp(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodeWebp(const RasterImage&, int, std::vector<uint8_t>&, std::string&);
bool
decodeTiff(const std::wstring&, RasterImage&, std::string&);
bool
encodeTiff(const std::wstring&, const RasterImage&, std::string&);
bool
decodePnm(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodePnm(const std::string&, const RasterImage&, std::vector<uint8_t>&, std::string&);
bool
decodePcx(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodePcx(const RasterImage&, std::vector<uint8_t>&, std::string&);
bool
decodeStb(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodeStb(const std::string&, const RasterImage&, int, std::vector<uint8_t>&, std::string&);
bool
decodeGif(const std::vector<uint8_t>&, RasterImage&, std::string&);
bool
encodeGif(const RasterImage&, std::vector<uint8_t>&, std::string&);
//=============================================================================
} // namespace Nelson
//=============================================================================
