//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_numericStateHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_finiteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = allNumericValuesSatisfy(argIn[0], true, false);
    std::wstring msg;
    if (!passed) {
        msg = resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAllEntries2",
            L"Assertion failed: expected all entries to be finite.");
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
