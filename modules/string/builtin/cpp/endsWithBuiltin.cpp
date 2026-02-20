//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "endsWithBuiltin.hpp"
#include "StringEndsWith.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::endsWithBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() != 2 && argIn.size() != 4) {
        raiseError2(_E("nelson:arguments:wrongNumberOfInputs"));
    }
    bool bCaseSensitive = true;
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        if (argIn.size() == 4) {
            ArrayOf param3 = argIn[2];
            std::wstring fieldname = param3.getContentAsWideString();
            if (fieldname != L"IgnoreCase") {
                raiseError2(_E("nelson:validators:invalidValueAtPosition"), 3);
            }
            ArrayOf param4 = argIn[3];
            logical fieldvalue = param4.getContentAsLogicalScalar();
            bCaseSensitive = (fieldvalue == 0);
        }
        retval << StringEndsWith(A, B, bCaseSensitive);
    } else {
        raiseError(L"Nelson:string:ERROR_CHAR_VECTOR_OR_CELL_EXPECTED",
            ERROR_CHAR_VECTOR_OR_CELL_EXPECTED);
    }
    return retval;
}
//=============================================================================
