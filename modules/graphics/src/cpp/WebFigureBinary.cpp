//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "WebFigureBinary.hpp"
#include <cstdint>
//=============================================================================
namespace Nelson {
//=============================================================================
std::string
webFigureBase64Encode(const void* data, size_t byteCount)
{
    static constexpr char alphabet[]
        = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    const auto* bytes = static_cast<const uint8_t*>(data);
    std::string encoded;
    encoded.reserve(((byteCount + 2) / 3) * 4);
    size_t index = 0;
    while (index + 2 < byteCount) {
        const uint32_t value = (static_cast<uint32_t>(bytes[index]) << 16)
            | (static_cast<uint32_t>(bytes[index + 1]) << 8)
            | static_cast<uint32_t>(bytes[index + 2]);
        encoded.push_back(alphabet[(value >> 18) & 0x3F]);
        encoded.push_back(alphabet[(value >> 12) & 0x3F]);
        encoded.push_back(alphabet[(value >> 6) & 0x3F]);
        encoded.push_back(alphabet[value & 0x3F]);
        index += 3;
    }
    if (index < byteCount) {
        uint32_t value = static_cast<uint32_t>(bytes[index]) << 16;
        const bool hasSecondByte = (index + 1 < byteCount);
        if (hasSecondByte) {
            value |= static_cast<uint32_t>(bytes[index + 1]) << 8;
        }
        encoded.push_back(alphabet[(value >> 18) & 0x3F]);
        encoded.push_back(alphabet[(value >> 12) & 0x3F]);
        encoded.push_back(hasSecondByte ? alphabet[(value >> 6) & 0x3F] : '=');
        encoded.push_back('=');
    }
    return encoded;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
