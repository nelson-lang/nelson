//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "traceBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "TraceMatrix.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::traceBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "trace", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType() || argIn[0].isSparse() || argIn[0].isLogical()
            || argIn[0].isCharacterArray() || argIn[0].isIntegerType()) {
            retval = OverloadFunction(eval, nLhs, argIn, "trace", bSuccess);
            if (bSuccess) {
                return retval;
            }
            OverloadRequired("trace");
        }
        retval << TraceMatrix(argIn[0]);
    }
    return retval;
}
//=============================================================================
