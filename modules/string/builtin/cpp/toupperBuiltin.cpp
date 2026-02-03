//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "toupperBuiltin.hpp"
#include "Error.hpp"
#include "ToUpper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::toupperBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf A = argIn[0];
    bool needToOverload;
    ArrayOf res = ToUpper(A, needToOverload);
    if (needToOverload) {
        raiseError(ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_CELL_EXPECTED, 1);
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
