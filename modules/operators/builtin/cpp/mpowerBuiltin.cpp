//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mpowerBuiltin.hpp"
#include "CheckerHelpers.hpp"
#include "OverloadFunction.hpp"
#include "MatrixPower.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::mpowerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "mpower", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = argIn[1];
        ArrayOf res = MatrixPower(param1, param2, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "mpower");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
