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
#include <vector>
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonEd25519.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson::CoreGateway {
//=============================================================================
inline bool
cryptoIsTextScalar(const ArrayOf& arg)
{
    return arg.isRowVectorCharacterArray() || arg.isScalarStringArray()
        || (arg.isCharacterArray() && arg.isEmpty());
}
//=============================================================================
inline bool
cryptoIsByteVector(const ArrayOf& arg)
{
    return arg.dataClass() == NLS_UINT8 && !arg.isSparse() && (arg.isEmpty() || arg.isVector());
}
//=============================================================================
// Message argument: uint8 vector (raw bytes) or text (UTF-8 bytes), or a
// filename when asFile is true (the file content is read as raw bytes).
inline std::vector<uint8_t>
cryptoMessageBytes(const ArrayOf& arg, bool asFile, int position)
{
    std::vector<uint8_t> bytes;
    if (asFile) {
        if (!cryptoIsTextScalar(arg)) {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", position, L"'char' or 'string'");
        }
        std::wstring filename = arg.asWideString();
        if (!readFileBytes(filename, bytes)) {
            Error(L"Nelson:core:cryptoFileNotReadable", L"File cannot be read: %s", filename);
        }
        return bytes;
    }
    if (cryptoIsTextScalar(arg)) {
        std::string utf8 = wstring_to_utf8(arg.asWideString());
        bytes.assign(utf8.begin(), utf8.end());
        return bytes;
    }
    if (cryptoIsByteVector(arg)) {
        const auto* data = static_cast<const uint8_t*>(arg.dataPointer());
        bytes.assign(data, data + arg.elementCount());
        return bytes;
    }
    Error(L"Nelson:types:wrongArgumentTypeExpected", L"Wrong type for argument #%d: %s expected.",
        position, L"'uint8' or 'char'");
    return bytes;
}
//=============================================================================
// Binary argument given as a uint8 vector (raw bytes) or as hexadecimal text.
// The caller checks the resulting size.
inline std::vector<uint8_t>
cryptoHexOrBytes(const ArrayOf& arg, int position)
{
    std::vector<uint8_t> bytes;
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::wstring hex = arg.asWideString();
        if (!hexStringToBytes(hex, bytes)) {
            Error(L"Nelson:core:ed25519HexStringExpected",
                L"#%d argument: hexadecimal characters expected.", position);
        }
    } else if (arg.dataClass() == NLS_UINT8 && !arg.isSparse() && arg.isVector()) {
        const auto* data = static_cast<const uint8_t*>(arg.dataPointer());
        bytes.assign(data, data + arg.elementCount());
    } else {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", position, L"'uint8' or 'char'");
    }
    return bytes;
}
//=============================================================================
// Fixed-size binary argument: uint8 vector of exactly expectedSize bytes, or a
// hexadecimal text of exactly 2 * expectedSize characters.
inline std::vector<uint8_t>
cryptoFixedBytes(const ArrayOf& arg, size_t expectedSize, const std::wstring& errorId,
    const std::wstring& what, int position)
{
    std::vector<uint8_t> bytes = cryptoHexOrBytes(arg, position);
    if (bytes.size() != expectedSize) {
        Error(errorId, L"%s must be %d bytes (uint8 vector) or %d hexadecimal characters.", what,
            static_cast<int>(expectedSize), static_cast<int>(expectedSize * 2));
    }
    return bytes;
}
//=============================================================================
inline bool
cryptoIsFileOption(const ArrayOf& arg, int position)
{
    if (!arg.isRowVectorCharacterArray() && !arg.isScalarStringArray()) {
        Error(L"Nelson:types:wrongArgumentTypeExpected",
            L"Wrong type for argument #%d: %s expected.", position, L"'char' or 'string'");
    }
    std::wstring option = arg.asWideString();
    if (option != L"-file") {
        Error(
            L"Nelson:core:cryptoFileOptionExpected", L"#%d argument: '-file' expected.", position);
    }
    return true;
}
//=============================================================================
} // namespace Nelson::CoreGateway
//=============================================================================
