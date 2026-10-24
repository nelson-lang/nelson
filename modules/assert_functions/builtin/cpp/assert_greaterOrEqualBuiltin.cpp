//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_relationHelpers.hpp"
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_greaterOrEqualBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring msg;
    bool passed = relationCheck(
        argIn[0], argIn[1], L"asserts.greaterOrEqual", L"computed >= expected", ge, msg);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
