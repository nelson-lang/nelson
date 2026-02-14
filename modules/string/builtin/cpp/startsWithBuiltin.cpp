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
#include "startsWithBuiltin.hpp"
#include "StringStartsWith.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::startsWithBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    if (argIn.size() != 2 && argIn.size() != 4) {
        raiseError2(L"Nelson:error_manager:wrong_rhs");
    }
    bool bCaseSensitive = true;
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        if (argIn.size() == 4) {
            ArrayOf param3 = argIn[2];
            std::wstring fieldname = param3.getContentAsWideString();
            if (fieldname != L"IgnoreCase") {
                raiseError2(L"Nelson:error_manager:wrong_value", 3);
            }
            ArrayOf param4 = argIn[3];
            logical fieldvalue = param4.getContentAsLogicalScalar();
            bCaseSensitive = (fieldvalue == 0);
        }
        retval << StringStartsWith(A, B, bCaseSensitive);
    } else {
        raiseError(L"Nelson:string:ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_CELL_EXPECTED",
            ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_CELL_EXPECTED, 1);
    }
    return retval;
}
//=============================================================================
