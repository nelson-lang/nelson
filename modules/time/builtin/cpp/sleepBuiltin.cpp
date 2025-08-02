//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sleepBuiltin.hpp"
#include "Sleep.hpp"
#include "OverloadRequired.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TimeGateway::sleepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf Parameter1 = argIn[0];
    if (Parameter1.isDoubleType()) {
        double dValue = Parameter1.getContentAsDoubleScalar();
        Sleep(eval, dValue);
    } else {
        OverloadRequired("sleep");
    }
    return retval;
}
//=============================================================================
