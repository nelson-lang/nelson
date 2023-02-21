//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "singleBuiltin.hpp"
#include "Error.hpp"
#include "ToSingle.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "CheckerHelpers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SingleGateway::singleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    ArrayOf A(argIn[0]);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "single", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = ToSingle(A, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "single", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE);
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
