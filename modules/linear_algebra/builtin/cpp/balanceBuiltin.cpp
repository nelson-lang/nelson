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
#include "PredefinedErrorMessages.hpp"
#include "characters_encoding.hpp"
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
            raiseError(L"Nelson:linear_algebra:ERROR_BALANCE_UNKNOWN_SECOND_ARGUMENT",
                ERROR_BALANCE_UNKNOWN_SECOND_ARGUMENT);
        }
        noperm = true;
    }
    retval = Balance(argIn[0], noperm, nLhs, needToOverload);
    if (needToOverload) {
        if (!argIn[0].is2D()) {
            raiseError(L"Nelson:linear_algebra:ERROR_BALANCE_INPUT_MUST_BE_2D",
                ERROR_BALANCE_INPUT_MUST_BE_2D);
        } else if (argIn[0].isSparse()) {
            raiseError(L"Nelson:linear_algebra:ERROR_BALANCE_NO_BALANCE_SPARSE",
                ERROR_BALANCE_NO_BALANCE_SPARSE);
        } else {
            raiseError(L"Nelson:linear_algebra:ERROR_BALANCE_INPUT_TYPE", ERROR_BALANCE_INPUT_TYPE);
        }
    }
    return retval;
}
//=============================================================================
