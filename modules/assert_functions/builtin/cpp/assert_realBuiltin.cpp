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
Nelson::AssertFunctionsGateway::assert_realBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    ArrayOf value = argIn[0];
    bool passed = value.allReal();
    if (value.isComplex() && passed && !value.isEmpty(true)) {
        passed = false;
    }
    std::wstring msg;
    if (!passed) {
        msg = formatI18nMessage(
            resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedRealComputed",
                L"Assertion failed: expected real value, computed %s."),
            formatClassAndSize(argIn[0]));
    }
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
