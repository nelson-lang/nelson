//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strjustBuiltin.hpp"
#include "StringJustify.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strjustBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    ArrayOf A = argIn[0];
    if (A.isEmpty()) {
        retval << A;
        return retval;
    }
    if ((A.isNumeric() || A.isLogical()) && !A.isSparse()) {
        A.promoteType(NLS_CHAR);
    }
    if (A.isCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        STRINGJUSTIFY side = STRINGJUSTIFY::NLS_JUSTIFY_LEFT;
        if (argIn.size() == 2) {
            std::wstring style = argIn[1].getContentAsWideString();
            if (style == L"left") {
                side = STRINGJUSTIFY::NLS_JUSTIFY_LEFT;
            } else if (style == L"center") {
                side = STRINGJUSTIFY::NLS_JUSTIFY_CENTER;
            } else if (style == L"right") {
                side = STRINGJUSTIFY::NLS_JUSTIFY_RIGHT;
            } else {
                Error(_W("Wrong value for #2 argument: 'left', 'right', 'center' expected."));
            }
        }
        retval << StringJustify(A, side);
    } else {
        OverloadRequired("strjust");
    }
    return retval;
}
//=============================================================================
