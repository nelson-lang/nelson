//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "hypotBuiltin.hpp"
#include "Error.hpp"
#include "Hypothenus.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::hypotBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "hypot", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = Hypothenuse(argIn[0], argIn[1], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "hypot");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
