//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_flowHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_notEqualBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring ignored;
    bool equal = Assert_IsEqual(eval, argIn[0], argIn[1], ignored);
    std::wstring msg = equal
        ? resolveMessageTemplate(L"Nelson:assert_functions:assertionFailedAssertsNotequal",
              L"Assertion failed: asserts.notEqual expected different values.")
        : L"";
    return assertionResult(nLhs, !equal, msg);
}
//=============================================================================
