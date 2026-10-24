//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "function_handle_isequalBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FunctionHandleIsEqual.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_isequalBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    if (!param1.isFunctionHandle()) {
        return ArrayOf::logicalScalar(false);
    }
    Dimensions dims1 = param1.dimensions();

    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOf param2 = argIn[k];
        if (!param2.isFunctionHandle()) {
            return ArrayOf::logicalScalar(false);
        }
        Dimensions dims2 = param2.dimensions();
        if (!dims1.equals(dims2)) {
            return ArrayOf::logicalScalar(false);
        }
        if (!FunctionHandleIsEqual(param1, param2)) {
            return ArrayOf::logicalScalar(false);
        }
    }
    return ArrayOf::logicalScalar(true);
}
//=============================================================================
