//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "base2decBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "BaseToDecimal.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::bin2decBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
        if (!bSuccess) {
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
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::hex2decBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
        if (!bSuccess) {
            bool needToOverload;
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = ArrayOf::doubleConstructor(16.);
            ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
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
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
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
        if (!bSuccess) {
            bool needToOverload;
            ArrayOf param1 = argIn[0];
            ArrayOf param2 = argIn[1];
            ArrayOf res = BaseToDecimal(param1, param2, needToOverload);
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
