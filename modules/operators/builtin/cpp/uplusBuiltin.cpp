//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uplusBuiltin.hpp"
#include "Error.hpp"
#include "UnaryPlus.hpp"
#include "OverloadUnaryOperator.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uplusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    ArrayOf arg1 = argIn[0];
    bool bSuccess = false;
    ArrayOf a = argIn[0];
    ArrayOf res;
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "uplus", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = UnaryPlus(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "uplus");
        } else {
            retval << res;
        }
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
