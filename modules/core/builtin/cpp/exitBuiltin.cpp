//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "exitBuiltin.hpp"
#include "Error.hpp"
#include "CheckerHelpers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::exitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    int iValue = 0;
    if (argIn.empty()) {
        iValue = 0;
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isDoubleType() || (param1.isSparse())) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
        } else {
            ArrayOf param1 = argIn[0];
            if (!param1.isScalar()) {
                Error(ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED);
            }
            double dValue = param1.getContentAsDoubleScalar();
            iValue = static_cast<int>(dValue);
#ifndef _MSC_VER
            if (iValue < 0 || iValue > 255) {
                Error(_("Value between 0 and 255 expected."));
            }

#endif
            if (static_cast<double>(iValue) != dValue) {
                Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
            }
        }
    }
    eval->setExitCode(iValue);
    eval->setState(NLS_STATE_QUIT);
    return retval;
}
//=============================================================================
