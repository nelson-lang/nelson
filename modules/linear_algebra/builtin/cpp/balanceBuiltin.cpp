//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "balanceBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Balance.hpp"
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
        std::wstring param2Str = argIn[1].getContentAsWideString();
        if (param2Str != L"noperm") {
            Error(_("Second argument must be 'noperm'."), "Nelson:balance:unknownSecondArgument");
        }
        noperm = true;
    }
    retval = Balance(argIn[0], noperm, nLhs, needToOverload);
    if (needToOverload) {
        if (!argIn[0].is2D()) {
            Error(_("Input must be 2-D."), "Nelson:balance:inputMustBe2D");
        } else if (argIn[0].isSparse()) {
            Error(_("Use balance(full(S))."), "Nelson:balance:noBalanceSparse");
        } else {
            Error(_("First argument must be single or double."), "Nelson:balance:inputType");
        }
    }
    return retval;
}
//=============================================================================
