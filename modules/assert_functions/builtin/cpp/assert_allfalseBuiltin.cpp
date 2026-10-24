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
Nelson::AssertFunctionsGateway::assert_allfalseBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = allLogicalValuesEqual(argIn[0], static_cast<logical>(0));
    std::wstring msg;
    if (!passed) {
        msg = resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAllEntries",
            L"Assertion failed: expected all entries to be false.");
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
