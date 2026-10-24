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
Nelson::AssertFunctionsGateway::assert_classBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring expected = argIn[1].asWideString();
    std::wstring computed;
    ClassName(argIn[0], computed);
    bool passed = computed == expected;
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedClassComputed",
                L"Assertion failed: expected class '%s', computed class '%s'."),
            expected, computed);
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
