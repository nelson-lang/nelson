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
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "sprintf", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf param1 = argIn[0];
        bool supported = param1.isCharacterArray() || (param1.isStringArray() && param1.isScalar());
        if (!supported) {
            retval = OverloadFunction(eval, nLhs, argIn, "sprintf", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
            }
            return retval;
        }
        if (!param1.isVector()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring result;
        std::wstring error_message;
        bool bRes = StringPrintf(result, error_message, eval, argIn);
        if (!bRes) {
            if (nLhs > 1) {
                Dimensions dims(1, 0);
                ArrayOf strArr = ArrayOf::emptyConstructor(dims);
                strArr.promoteType(NLS_CHAR);
                retval << strArr;
                retval << ArrayOf::characterArrayConstructor(error_message);
            } else {
                Error(error_message);
            }
        } else {
            if (result.empty()) {
                if (param1.getDataClass() == NLS_CHAR) {
                    Dimensions dims(1, 0);
                    ArrayOf strArr = ArrayOf::emptyConstructor(dims);
                    strArr.promoteType(NLS_CHAR);
                    retval << strArr;
                } else {
                    retval << ArrayOf::stringArrayConstructor(result);
                }
            } else {
                if (param1.getDataClass() == NLS_CHAR) {
                    retval << ArrayOf::characterArrayConstructor(result);
                } else {
                    retval << ArrayOf::stringArrayConstructor(result);
                }
            }
            if (nLhs > 1) {
                retval << ArrayOf::characterArrayConstructor(L"");
            }
        }
    }
    return retval;
}
//=============================================================================
