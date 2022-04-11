//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "uminusBuiltin.hpp"
#include "Error.hpp"
#include "UnaryMinus.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::uminusBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    ArrayOf a = argIn[0];
    ArrayOf res;
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "uminus", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = UnaryMinus(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "uminus");
        } else {
            retval << res;
        }
    } else {
        retval << res;
    }
    return retval;
}
//=============================================================================
