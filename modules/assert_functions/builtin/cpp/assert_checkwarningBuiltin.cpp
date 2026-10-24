//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_warningHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_checkwarningBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    std::wstring command = argIn[0].asWideString();
    std::wstring expected = argIn[1].asWideString();
    if (expected.empty()) {
        Error(L"Nelson:asserts:invalidArgument:emptyExpectedWarning",
            L"empty string not allowed as expected warning message or identifier.");
    }
    std::wstring msg;
    bool passed = false;
    if (argIn.size() == 3) {
        std::wstring expectedid = argIn[2].asWideString();
        passed = Assert_CheckWarning(eval, command, expected, expectedid, msg);
    } else {
        passed = Assert_CheckWarning(eval, command, expected, msg);
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
