//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_basicShapeHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_notemptyBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = !argIn[0].isEmpty();
    std::wstring msg;
    if (!passed) {
        msg = resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedNonEmpty",
            L"Assertion failed: expected non-empty value.");
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
