//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "base64decodeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
// __base64decode__(str): decode a base64 character vector / string to a uint8
// row vector.
ArrayOfVector
Nelson::CharactersEncodingGateway::base64decodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (!param1.isCharacterArray() && !param1.isStringArray()) {
        Error(L"Nelson:base64decode:invalidInput",
            L"First argument must be a character vector or string scalar.");
    }
    std::string encoded = param1.asUtf8String();
    bool ok = true;
    std::string bytes = base64_decode(encoded, ok);
    if (!ok) {
        Error(L"Nelson:base64decode:notValidBase64", L"Input is not valid base64.");
    }
    Dimensions dims(1, bytes.size());
    uint8* values = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, bytes.size());
    if (!bytes.empty()) {
        memcpy(values, bytes.data(), bytes.size());
    }
    return ArrayOfVector { ArrayOf(NLS_UINT8, dims, values) };
}
//=============================================================================
