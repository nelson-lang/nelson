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
Nelson::AssertFunctionsGateway::assert_columnsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    indexType expected = 0;
    if (!indexScalar(argIn[1], expected)) {
        Error(L"Nelson:asserts:invalidArgument:columnCount",
            L"Expected column count must be a nonnegative finite integer scalar.");
    }
    indexType computed = argIn[0].columns();
    bool passed = computed == expected;
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsColumns",
                  L"Assertion failed: asserts.columns expected %d, computed %d."),
              static_cast<unsigned long long>(expected), static_cast<unsigned long long>(computed));
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
