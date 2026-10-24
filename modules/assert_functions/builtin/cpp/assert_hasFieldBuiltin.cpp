//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_structHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_hasFieldBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isStruct()) {
        Error(L"Nelson:asserts:invalidStruct", L"Struct value expected.");
    }
    std::string expected = asFieldName(argIn[1]);
    bool passed = containsField(argIn[0].fieldNames(), expected);
    std::wstring wideExpected(expected.begin(), expected.end());
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsHasfield",
                  L"Assertion failed: asserts.hasField expected field '%s'."),
              wideExpected);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
