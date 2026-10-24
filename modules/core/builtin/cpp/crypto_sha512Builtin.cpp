//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "crypto_sha512Builtin.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonCryptoHash.hpp"
#include "NelsonEd25519.hpp"
#include "cryptoArgumentsHelpers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::CoreGateway;
//=============================================================================
enum class SHA512_INPUT_MODE
{
    FILENAME,
    STRING,
    AUTO
};
//=============================================================================
static std::wstring
sha512OfText(const std::wstring& text, SHA512_INPUT_MODE mode);
static ArrayOf
sha512OfArgument(const ArrayOf& arg, SHA512_INPUT_MODE mode);
//=============================================================================
// hexa_hash = crypto.sha512(str | filename | uint8_bytes)
// hexa_hash = crypto.sha512(str, '-file')
// hexa_hash = crypto.sha512(str, '-string')
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::crypto_sha512Builtin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    SHA512_INPUT_MODE mode = SHA512_INPUT_MODE::AUTO;
    if (argIn.size() == 2) {
        if (!cryptoIsTextScalar(argIn[1])) {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", 2, L"'char' or 'string'");
        }
        std::wstring option = argIn[1].asWideString();
        if (option == L"-file") {
            mode = SHA512_INPUT_MODE::FILENAME;
        } else if (option == L"-string") {
            mode = SHA512_INPUT_MODE::STRING;
        } else {
            Error(L"Nelson:core:cryptoInputOptionExpected",
                L"#%d argument: '-file' or '-string' expected.", 2);
        }
    }
    retval << sha512OfArgument(argIn[0], mode);
    return retval;
}
//=============================================================================
static std::wstring
sha512OfText(const std::wstring& text, SHA512_INPUT_MODE mode)
{
    bool asFile = (mode == SHA512_INPUT_MODE::FILENAME)
        || (mode == SHA512_INPUT_MODE::AUTO && FileSystemWrapper::Path::is_regular_file(text));
    if (asFile) {
        std::vector<uint8_t> bytes;
        if (!readFileBytes(text, bytes)) {
            return L"";
        }
        return computeBytesToSHA512Hex(bytes.data(), bytes.size());
    }
    std::string utf8 = wstring_to_utf8(text);
    return computeBytesToSHA512Hex(reinterpret_cast<const uint8_t*>(utf8.data()), utf8.size());
}
//=============================================================================
static ArrayOf
sha512OfArgument(const ArrayOf& arg, SHA512_INPUT_MODE mode)
{
    if (cryptoIsByteVector(arg)) {
        if (mode == SHA512_INPUT_MODE::FILENAME) {
            Error(L"Nelson:types:wrongArgumentTypeExpected",
                L"Wrong type for argument #%d: %s expected.", 1, L"'char' or 'string'");
        }
        const auto* data = static_cast<const uint8_t*>(arg.dataPointer());
        return ArrayOf::characterArray(computeBytesToSHA512Hex(data, arg.elementCount()));
    }
    if (arg.isStringArray() || arg.isCellArrayOfCharacterVectors()) {
        Dimensions dims = arg.dimensions();
        size_t count = dims.elementCount();
        NelsonType type = arg.isStringArray() ? NLS_STRING_ARRAY : NLS_CELL_ARRAY;
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(type, count);
        ArrayOf res(type, dims, elements);
        auto* cells = (ArrayOf*)(arg.dataPointer());
        for (size_t k = 0; k < count; k++) {
            if (cells[k].isCharacterArray()) {
                elements[k] = ArrayOf::characterArray(sha512OfText(cells[k].asWideString(), mode));
            } else {
                elements[k] = ArrayOf::empty();
            }
        }
        return res;
    }
    if (cryptoIsTextScalar(arg)) {
        return ArrayOf::characterArray(sha512OfText(arg.asWideString(), mode));
    }
    Error(L"Nelson:types:wrongArgumentTypeExpected", L"Wrong type for argument #%d: %s expected.",
        1, L"'uint8', 'char', 'string' or 'cell'");
    return {};
}
//=============================================================================
