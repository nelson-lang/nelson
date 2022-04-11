//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "fullBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::fullBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1); // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "full", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isReferenceType()) {
            retval = OverloadFunction(eval, nLhs, argIn, "full", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(_W("Undefined function 'full' for input arguments."));
        }
        ArrayOf R(argIn[0]);
        try {
            R.makeDense();
            retval << R;
        } catch (const Exception&) {
            retval = OverloadFunction(eval, nLhs, argIn, "full", bSuccess);
            if (bSuccess) {
                return retval;
            }
            Error(_W("Undefined function 'full' for input arguments."));
        }
    }
    return retval;
}
//=============================================================================
