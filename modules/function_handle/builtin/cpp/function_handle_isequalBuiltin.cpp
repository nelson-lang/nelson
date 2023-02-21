//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "function_handle_isequalBuiltin.hpp"
#include "CheckerHelpers.hpp"
#include "FunctionHandleIsEqual.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 2, 2);
    ArrayOfVector retval(1);
    retval << ArrayOf::logicalConstructor(FunctionHandleIsEqual(argIn[0], argIn[1]));
    return retval;
}
//=============================================================================
