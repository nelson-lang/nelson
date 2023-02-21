//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "logBuiltin.hpp"
#include "Error.hpp"
#include "NaturalLogarithm.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::logBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "log", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = NaturalLogarithm(argIn[0], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "log");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
