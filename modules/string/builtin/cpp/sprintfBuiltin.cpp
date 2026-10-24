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
#include "ErrorID.hpp"
#include "StringPrintf.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "OverloadHelpers.hpp"
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
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (!param1.isVector() && !param1.isEmpty()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    std::wstring result;
    std::wstring error_message;
    ArrayOfVector printfArgs = convertObjectPrintfArguments(eval, argIn);
    bool bRes = StringPrintf(result, error_message, eval, printfArgs);
    if (!bRes) {
        if (nLhs > 1) {
            Dimensions dims(1, 0);
            ArrayOf strArr = ArrayOf::empty(dims);
            strArr.promoteType(NLS_CHAR);
            retval << strArr;
            retval << ArrayOf::characterArray(error_message);
        } else {
            Error(L"Nelson:sprintf:formatFailed", error_message);
        }
    } else {
        if (result.empty()) {
            if (param1.dataClass() == NLS_CHAR) {
                Dimensions dims(1, 0);
                ArrayOf strArr = ArrayOf::empty(dims);
                strArr.promoteType(NLS_CHAR);
                retval << strArr;
            } else {
                retval << ArrayOf::stringArray(result);
            }
        } else {
            if (param1.dataClass() == NLS_CHAR) {
                retval << ArrayOf::characterArray(result);
            } else {
                retval << ArrayOf::stringArray(result);
            }
        }
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(L"");
        }
    }
    return retval;
}
//=============================================================================
