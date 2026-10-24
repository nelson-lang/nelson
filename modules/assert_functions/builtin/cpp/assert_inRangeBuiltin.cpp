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
Nelson::AssertFunctionsGateway::assert_inRangeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 2);
    std::wstring lowerMsg;
    std::wstring upperMsg;
    bool lower = relationCheck(
        argIn[0], argIn[1], L"asserts.inRange", L"computed >= minimum", ge, lowerMsg);
    bool upper = relationCheck(
        argIn[0], argIn[2], L"asserts.inRange", L"computed <= maximum", le, upperMsg);
    return assertionResult(nLhs, lower && upper, lower ? upperMsg : lowerMsg);
}
//=============================================================================
