//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "tanhBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "TrigonometricFunctions.hpp"
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TrigonometricGateway::tanhBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "tanh", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = Tanh(argIn[0], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "tanh", bSuccess);
            if (!bSuccess) {
                Error(_("Undefined function 'tanh' for input arguments of type") + " '"
                    + ClassName(argIn[0]) + "'.");
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
