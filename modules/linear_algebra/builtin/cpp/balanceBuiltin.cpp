//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "balanceBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Balance.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::balanceBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 3);
    bool needToOverload;
    bool noperm = false;
    ArrayOf param1 = argIn[0];
    if (argIn.size() == 2) {
        std::wstring param2Str = argIn[1].asWideString();
        if (param2Str != L"noperm") {
            Error("Nelson:balance:unknownSecondArgument", "Second argument must be 'noperm'.");
        }
        noperm = true;
    }
    retval = Balance(argIn[0], noperm, nLhs, needToOverload);
    if (needToOverload) {
        if (!argIn[0].is2D()) {
            Error("Nelson:balance:inputMustBe2D", "Input must be 2-D.");
        } else if (argIn[0].isSparse()) {
            Error("Nelson:balance:noBalanceSparse", "Use balance(full(S)).");
        } else {
            Error("Nelson:balance:inputType", "First argument must be single or double.");
        }
    }
    return retval;
}
//=============================================================================
