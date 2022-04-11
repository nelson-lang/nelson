//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "transposeBuiltin.hpp"
#include "Error.hpp"
#include "Transpose.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OperatorsGateway::transposeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    ArrayOf res;
    ArrayOf a = argIn[0];
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "transpose", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = Transpose(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "transpose", bSuccess);
        }
    }
    retval << res;
    return retval;
}
//=============================================================================
