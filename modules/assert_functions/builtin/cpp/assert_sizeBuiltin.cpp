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
Nelson::AssertFunctionsGateway::assert_sizeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[1].isNumeric() || argIn[1].isComplex() || argIn[1].isSparse()
        || !argIn[1].isVector()) {
        Error(L"Nelson:asserts:invalidArgument:sizeVector",
            L"Expected size must be a numeric vector.");
    }
    Dimensions actual = argIn[0].dimensions();
    std::vector<double> expected = argIn[1].asDoubleVector();
    bool passed = expectedSizeMatches(actual, expected);
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedSizeComputed",
                L"Assertion failed: expected size %s, computed size %s."),
            formatExpectedSize(expected), formatSize(actual));
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
