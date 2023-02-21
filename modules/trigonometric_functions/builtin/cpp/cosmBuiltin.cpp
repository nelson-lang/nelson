//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cosmBuiltin.hpp"
#include "Error.hpp"
#include "MatrixCosinus.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TrigonometricGateway::cosmBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "cosm", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = MatrixCos(argIn[0], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "cosm", bSuccess);
            if (!bSuccess) {
                Error(_("Undefined function 'cosm' for input arguments of type") + " '"
                    + ClassName(argIn[0]) + "'.");
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
