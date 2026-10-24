//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_errorHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_checkerror_containsBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 2);
    std::wstring command = argIn[0].asWideString();
    std::wstring expectedSubstring = argIn[1].asWideString();
    if (expectedSubstring.empty()) {
        Error(L"Nelson:asserts:invalidArgument:emptyExpectedSubstring",
            L"empty string not allowed as expected substring.");
    }
    std::wstring msg;
    bool passed = false;
    if (argIn.size() == 3) {
        std::wstring expectedid = argIn[2].asWideString();
        passed = Assert_CheckErrorContains(eval, command, expectedSubstring, expectedid, msg);
    } else {
        passed = Assert_CheckErrorContains(eval, command, expectedSubstring, msg);
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
