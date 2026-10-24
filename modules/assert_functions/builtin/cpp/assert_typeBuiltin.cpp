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
Nelson::AssertFunctionsGateway::assert_typeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    wstringVector expectedTypes = getExpectedTypes(argIn[1]);
    if (expectedTypes.empty()) {
        Error(L"Nelson:asserts:invalidArgument:emptyTypeList",
            L"Expected type list must not be empty.");
    }
    std::wstring computed;
    ClassName(argIn[0], computed);
    bool passed = containsString(expectedTypes, computed);
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedClassOne",
                L"Assertion failed: expected class one of '%s', computed class '%s'."),
            joinWideStrings(expectedTypes), computed);
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
