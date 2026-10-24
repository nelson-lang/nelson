//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_shapeHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_lengthBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    indexType expected = 0;
    if (!indexScalar(argIn[1], expected)) {
        Error(L"Nelson:asserts:invalidArgument:length",
            L"Expected length must be a nonnegative finite integer scalar.");
    }
    Dimensions dims = argIn[0].dimensions();
    indexType computed = 0;
    for (indexType k = 0; k < dims.length(); ++k) {
        computed = std::max(computed, dims.dimensionLength(k));
    }
    bool passed = computed == expected;
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsLength",
                  L"Assertion failed: asserts.length expected %d, computed %d."),
              static_cast<unsigned long long>(expected), static_cast<unsigned long long>(computed));
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
