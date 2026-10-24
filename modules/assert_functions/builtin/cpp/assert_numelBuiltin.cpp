//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_sizeTypeHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_numelBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    indexType expected = 0;
    if (!validateElementCount(argIn[1], expected)) {
        Error(L"Nelson:asserts:invalidArgument:numelCount",
            L"Expected number of elements must be a nonnegative finite integer scalar.");
    }
    indexType computed = argIn[0].elementCount();
    bool passed = computed == expected;
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedElementsComputed",
                L"Assertion failed: expected %d elements, computed %d elements."),
            static_cast<unsigned long long>(expected), static_cast<unsigned long long>(computed));
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
