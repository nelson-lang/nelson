//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "num2binBuiltin.hpp"
#include "NumToBin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::num2binBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "num2bin", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassType()) {
            retval = OverloadFunction(eval, nLhs, argIn, "num2bin", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        nargincheck(argIn, 1, 1);
        if (!bSuccess) {
            bool needToOverload;
            ArrayOf res = NumToBin(argIn[0], needToOverload);
            if (needToOverload) {
                retval = OverloadFunction(eval, nLhs, argIn, "num2bin");
            } else {
                retval << res;
            }
        }
    }
    return retval;
}
//=============================================================================
