//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gcdBuiltin.hpp"
#include "Error.hpp"
#include "GCD.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SpecialFunctionsGateway::gcdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "gcd", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        ArrayOf res = GCD(argIn[0], argIn[1], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "gcd");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
