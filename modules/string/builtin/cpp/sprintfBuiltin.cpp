//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "sprintfBuiltin.hpp"
#include "Error.hpp"
#include "StringPrintf.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::sprintfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 2) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "sprintf", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        if (!param1.isString()) {
            retval = OverloadFunction(eval, nLhs, argIn, "sprintf", bSuccess);
            if (!bSuccess) {
                Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
            return retval;
        }
        if (!param1.isVector()) {
            Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring result = L"";
        std::wstring error_message = L"";
        bool bRes = StringPrintf(result, error_message, eval, argIn);
        if (!bRes) {
            if (nLhs > 1) {
                ArrayOf strArr = ArrayOf::emptyConstructor(Dimensions(1, 0));
                strArr.promoteType(NLS_CHAR);
                retval.push_back(strArr);
                retval.push_back(ArrayOf::stringConstructor(error_message));
            } else {
                Error(eval, error_message);
            }
        } else {
            if (result == L"") {
                ArrayOf strArr = ArrayOf::emptyConstructor(Dimensions(1, 0));
                strArr.promoteType(NLS_CHAR);
                retval.push_back(strArr);
            } else {
                retval.push_back(ArrayOf::stringConstructor(result));
            }
            if (nLhs > 1) {
                retval.push_back(ArrayOf::stringConstructor(L""));
            }
        }
    }
    return retval;
}
//=============================================================================
