//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "base2decBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "BaseToDecimal.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::bin2decBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "bin2dec", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "bin2dec", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        bool needToOverload;
        ArrayOf param1 = argIn[0];
        ArrayOf param2 = ArrayOf::doubleConstructor(2.);
        ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "bin2dec");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::hex2decBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "hex2dec", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "hex2dec", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        if (!bSuccess) { //-V547
            bool needToOverload;
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = ArrayOf::doubleConstructor(16.);
            ArrayOf res = BaseToDecimal(param1, param2, needToOverload); //-V821
            if (needToOverload) {
                retval = OverloadFunction(eval, nLhs, argIn, "hex2dec");
            } else {
                retval << res;
            }
        }
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::base2decBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "base2dec", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "base2dec", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        if (!bSuccess) { //-V547
            bool needToOverload;
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = argIn[1];
            ArrayOf res = BaseToDecimal(param1, param2, needToOverload); //-V821
            if (needToOverload) {
                retval = OverloadFunction(eval, nLhs, argIn, "base2dec");
            } else {
                retval << res;
            }
        }
    }
    return retval;
}
//=============================================================================
