//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "strlengthBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "StringLength.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strlengthBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf param = argIn[0];
    switch (param.dataClass()) {
    case NLS_CHAR:
    case NLS_STRING_ARRAY: {
        retval << StringLength(argIn[0]);
    } break;
    default:
    case NLS_CELL_ARRAY: {
        if (param.isCellArrayOfCharacterVectors()) {
            retval << StringLength(argIn[0]);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string or cell");
        }
    } break;
    }
    return retval;
}
//=============================================================================
