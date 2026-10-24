//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "assert_isapproxBuiltin.hpp"
#include "Assert_IsApprox.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_isapproxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 5);
    nargoutcheck(nLhs, 0, 2);
    ArrayOfVector retval(nLhs);
    double relativeTolerance = 0.;
    double absoluteTolerance = 0.;
    std::wstring customMessage;
    if (argIn.size() == 3) {
        ArrayOf param3 = argIn[2];
        if (param3.isCharacterArray() || param3.isStringArray()) {
            customMessage = param3.asWideString();
        } else if (!param3.isNumeric() || param3.isComplex() || param3.isSparse()
            || !param3.isScalar()) {
            Error(L"Nelson:asserts:invalidTolerance",
                L"Precision must be a nonnegative finite numeric scalar.");
        } else {
            relativeTolerance = param3.asDoubleScalar();
            if (!std::isfinite(relativeTolerance) || relativeTolerance < 0.) {
                Error(L"Nelson:asserts:invalidTolerance",
                    L"Precision must be a nonnegative finite numeric scalar.");
            }
        }
    }
    if (argIn.size() >= 4) {
        ArrayOf param3 = argIn[2];
        ArrayOf param4 = argIn[3];
        if (!param3.isNumeric() || param3.isComplex() || param3.isSparse() || !param3.isScalar()) {
            Error(L"Nelson:asserts:invalidTolerance",
                L"Precision must be a nonnegative finite numeric scalar.");
        }
        relativeTolerance = param3.asDoubleScalar();
        if (!std::isfinite(relativeTolerance) || relativeTolerance < 0.) {
            Error(L"Nelson:asserts:invalidTolerance",
                L"Precision must be a nonnegative finite numeric scalar.");
        }
        if (param4.isCharacterArray() || param4.isStringArray()) {
            customMessage = param4.asWideString();
        } else if (!param4.isNumeric() || param4.isComplex() || param4.isSparse()
            || !param4.isScalar()) {
            Error(L"Nelson:asserts:invalidTolerance:absolute",
                L"Absolute tolerance must be a nonnegative finite numeric scalar.");
        } else {
            absoluteTolerance = param4.asDoubleScalar();
            if (!std::isfinite(absoluteTolerance) || absoluteTolerance < 0.) {
                Error(L"Nelson:asserts:invalidTolerance:absolute",
                    L"Absolute tolerance must be a nonnegative finite numeric scalar.");
            }
        }
    }
    if (argIn.size() == 5) {
        customMessage = argIn[4].asWideString();
    }
    ArrayOf param1 = argIn[0];
    ArrayOf param2 = argIn[1];
    std::wstring msg;
    bool bRes = Assert_IsApprox(eval, param1, param2, relativeTolerance, absoluteTolerance, msg);
    if (!bRes && !customMessage.empty()) {
        msg = customMessage + L" " + msg;
    }
    if (nLhs == 0) {
        if (!bRes) {
            ErrorAsCaller(L"Nelson:asserts:assertionFailed", msg);
        }
    } else {
        retval << ArrayOf::logicalScalar(bRes);
        if (nLhs > 1) {
            retval << ArrayOf::characterArray(msg);
        }
    }
    return retval;
}
//=============================================================================
