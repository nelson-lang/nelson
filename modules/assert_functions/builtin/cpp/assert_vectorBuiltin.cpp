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
Nelson::AssertFunctionsGateway::assert_vectorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool passed = argIn[0].isVector();
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedVectorComputed",
                L"Assertion failed: expected vector value, computed size %s."),
            formatSize(argIn[0].dimensions()));
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
