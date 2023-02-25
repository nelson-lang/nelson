//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismemberBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "IsMember.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::ismemberBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "ismember", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        ArrayOf res = IsMember(argIn[0], argIn[1], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "ismember");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
