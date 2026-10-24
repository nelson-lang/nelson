//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_textHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_matchBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring text = argIn[0].asWideString();
    std::wstring pattern = argIn[1].asWideString();
    bool passed = false;
    try {
        passed = std::regex_search(text, std::wregex(pattern));
    } catch (const std::regex_error&) {
        Error(L"Nelson:asserts:invalidRegularExpression", L"Invalid regular expression.");
    }
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedTextDoes2",
                L"Assertion failed: text does not match regular expression '%s'."),
            pattern);
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
