//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_stringHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_endsWithBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring text = argIn[0].asWideString();
    std::wstring suffix = argIn[1].asWideString();
    bool passed = suffix.size() <= text.size()
        && text.compare(text.size() - suffix.size(), suffix.size(), suffix) == 0;
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsEndswith",
                  L"Assertion failed: asserts.endsWith expected suffix '%s'."),
              suffix);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
