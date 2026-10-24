//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "assert_extraBuiltins.hpp"
#include "Assert_IsEqual.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::AssertFunctionsGateway;
//=============================================================================
ArrayOfVector
Nelson::AssertFunctionsGateway::assert_diffBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring msg;
    bool passed = Assert_IsEqual(eval, argIn[0], argIn[1], msg);
    ArrayOfVector retval(nLhs);
    if (nLhs == 1) {
        retval << ArrayOf::characterArray(msg);
    } else if (nLhs > 1) {
        retval << ArrayOf::logicalScalar(passed);
        retval << ArrayOf::characterArray(msg);
    }
    return retval;
}
//=============================================================================
