//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "atanBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "TrigonometricFunctions.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::TrigonometricGateway::atanBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "atan", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = Atan(argIn[0], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "atan", bSuccess);
            if (!bSuccess) {
                Error(_("Undefined function 'atan' for input arguments of type") + " '"
                    + ClassName(argIn[0]) + "'.");
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
