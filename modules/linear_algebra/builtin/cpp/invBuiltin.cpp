//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "invBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "InverseMatrix.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::invBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "inv", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf param1 = argIn[0];
        ArrayOf res = InverseMatrix(param1, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "inv");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
