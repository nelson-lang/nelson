//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bin2numBuiltin.hpp"
#include "BinToNum.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::bin2numBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "bin2num", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "bin2num", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        nargincheck(argIn, 1, 1);
        if (!bSuccess) {
            bool needToOverload;
            ArrayOf res = BinToNum(argIn[0], needToOverload);
            if (needToOverload) {
                retval = OverloadFunction(eval, nLhs, argIn, "bin2num");
            } else {
                retval << res;
            }
        }
    }
    return retval;
}
//=============================================================================
