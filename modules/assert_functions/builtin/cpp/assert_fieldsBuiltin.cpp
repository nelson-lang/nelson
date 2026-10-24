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
Nelson::AssertFunctionsGateway::assert_fieldsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isStruct()) {
        Error(L"Nelson:asserts:invalidStruct", L"Struct value expected.");
    }
    stringVector actual = argIn[0].fieldNames();
    stringVector expected = fieldList(argIn[1]);
    std::sort(actual.begin(), actual.end());
    std::sort(expected.begin(), expected.end());
    bool passed = actual == expected;
    std::wstring msg = passed
        ? L""
        : formatI18nMessage(
              resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsFields",
                  L"Assertion failed: asserts.fields expected fields '%s', computed fields '%s'."),
              joinFields(expected), joinFields(actual));
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
