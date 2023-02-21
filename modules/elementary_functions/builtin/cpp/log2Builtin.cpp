//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "log2Builtin.hpp"
#include "Error.hpp"
#include "Logarithm2.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::log2Builtin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "log2", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        if (nLhs < 2) {
            retval << Logarithm2(argIn[0], needToOverload);
        } else {
            retval = Frexp(argIn[0], needToOverload);
        }
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "log2");
        }
    }
    return retval;
}
//=============================================================================
