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
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_noErrorBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    std::wstring msg;
    bool passed = Assert_NoError(eval, argIn[0].asWideString(), msg);
    return assertionResult(nLhs, passed, msg);
}
//=============================================================================
