//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "base64encodeBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <string>
//=============================================================================
using namespace Nelson;
//=============================================================================
// __base64encode__(data): base64 of a byte array (numeric/logical -> raw bytes)
// or of the UTF-8 bytes of a char/string. Returns a character row vector.
ArrayOfVector
Nelson::CharactersEncodingGateway::base64encodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf param1 = argIn[0];
    if (param1.isSparse()) {
        Error(L"Nelson:base64encode:sparseNotSupported", L"Sparse type not supported.");
    }
    std::string bytes;
    if (param1.isCharacterArray() || param1.isStringArray()) {
        bytes = param1.asUtf8String();
    } else if (param1.isNumeric() || param1.isLogical()) {
        param1.promoteType(NLS_UINT8);
        indexType len = param1.elementCount();
        if (len > 0) {
            const char* data = reinterpret_cast<const char*>(param1.dataPointer());
            bytes = std::string(data, (size_t)len);
        }
    } else {
        Error(L"Nelson:base64encode:invalidInput",
            L"First argument must be a numeric, logical or character array.");
    }
    return ArrayOfVector { ArrayOf::characterArray(base64_encode(bytes)) };
}
//=============================================================================
