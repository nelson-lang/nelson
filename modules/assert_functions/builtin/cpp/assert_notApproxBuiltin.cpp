//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_flowHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_notApproxBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 2);
    double relTol = argIn[2].asDoubleScalar();
    double absTol = argIn.size() == 4 ? argIn[3].asDoubleScalar() : 0.;
    if (!std::isfinite(relTol) || relTol < 0. || !std::isfinite(absTol) || absTol < 0.) {
        Error(L"Nelson:asserts:invalidTolerance:tolerance",
            L"Tolerance must be a nonnegative finite numeric scalar.");
    }
    std::wstring ignored;
    bool approx = Assert_IsApprox(eval, argIn[0], argIn[1], relTol, absTol, ignored);
    std::wstring msg = approx
        ? resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsNotapprox",
              L"Assertion failed: asserts.notApprox expected values to be different.")
        : L"";
    return assertionResult(nLhs, !approx, msg);
}
//=============================================================================
