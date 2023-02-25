//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "sleepBuiltin.hpp"
#include "Sleep.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
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
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "sleep", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf Parameter1 = argIn[0];
        if (Parameter1.isDoubleType()) {
            double dValue = Parameter1.getContentAsDoubleScalar();
            Sleep(eval, dValue);
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "sleep");
        }
    }
    return retval;
}
//=============================================================================
