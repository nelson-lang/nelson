//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "native2unicodeBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CharactersEncodingGateway::native2unicodeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOf param1 = argIn[0];
    if (param1.isSparse()) {
        Error(_W("Sparse type not supported."));
    }
    ArrayOfVector retval(1);
    std::string data;
    bool isSupportedAsNumeric
        = param1.isNumeric() && (param1.isEmpty() || param1.isScalar() || param1.isRowVector());
    bool isSupportedAsCharacterArray = param1.isCharacterArray()
        && (param1.isEmpty() || param1.isScalar() || param1.isRowVector());
    bool isSupportedAsScalarString
        = param1.isStringArray() && (param1.isEmpty() || param1.isScalar());
    if (isSupportedAsNumeric || isSupportedAsCharacterArray || isSupportedAsScalarString) {
        if (isSupportedAsNumeric) {
            param1.promoteType(NLS_UINT8);
            indexType l = param1.getElementCount();
            uint8* s = (uint8*)param1.getDataPointer();
            const char* chars = reinterpret_cast<const char*>(s);
            data = std::string(chars);
            data[l] = 0;
        } else {
            data = param1.getContentAsCString();
        }
    } else {
        Error("Type or dimensions not supported.");
    }
    std::string charset;
    if (argIn.size() == 2) {
        ArrayOf param2 = argIn[1];
        charset = param2.getContentAsCString();
        if (charset.empty()) {
            charset = getSystemEncoding();
        }
    } else {
        charset = getSystemEncoding();
    }
    std::string output;
    if (!charsetToUtf8Converter(data, charset, output)) {
        Error(_W("Cannot convert to unicode."));
    }
    retval << ArrayOf::characterArrayConstructor(output);
    return retval;
}
//=============================================================================
