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
#include <fmt/printf.h>
#include <fmt/format.h>
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
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bCaseSensitive = true;
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        if (argIn.size() == 4) {
            ArrayOf param3 = argIn[2];
            std::wstring fieldname = param3.getContentAsWideString();
            if (fieldname != L"IgnoreCase") {
                Error(fmt::sprintf(ERROR_WRONG_ARGUMENT_X_VALUE, 3));
            }
            ArrayOf param4 = argIn[3];
            logical fieldvalue = param4.getContentAsLogicalScalar();
            bCaseSensitive = (fieldvalue == 0);
        }
        retval << StringStartsWith(A, B, bCaseSensitive);
    } else {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
    }
    return retval;
}
//=============================================================================
