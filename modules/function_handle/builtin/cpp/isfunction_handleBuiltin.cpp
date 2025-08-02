//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "isfunction_handleBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringToFunctionHandle.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::isfunction_handleBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    if (arg1.isFunctionHandle()) {
        retval << ArrayOf::logicalConstructor(true);
    } else {
        retval << ArrayOf::logicalConstructor(false);
    }
    return retval;
}
//=============================================================================
