//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ctransposeBuiltin.hpp"
#include "Error.hpp"
#include "OverloadUnaryOperator.hpp"
#include "ComplexTranspose.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::ctransposeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    ArrayOf res;
    ArrayOf a = argIn[0];
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "ctranspose", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = ComplexTranspose(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "ctranspose", bSuccess);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
