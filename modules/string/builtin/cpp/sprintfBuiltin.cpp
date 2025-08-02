//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sprintfBuiltin.hpp"
#include "Error.hpp"
#include "StringPrintf.hpp"
#include "PredefinedErrorMessages.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::sprintfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 1);
    ArrayOf param1 = argIn[0];
    bool supported = param1.isCharacterArray() || (param1.isStringArray() && param1.isScalar());
    if (!supported) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
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
    return retval;
}
//=============================================================================
