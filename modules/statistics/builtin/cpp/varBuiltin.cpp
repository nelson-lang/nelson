//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "varBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "Variance.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::varBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool bSuccess = false;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 1);
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "var", bSuccess);
    }
    if (!bSuccess) {
        ArrayOf A = argIn[0];
        int w = 0;
        int d = -1;
        if (argIn.size() > 1) {
            ArrayOf arg2 = argIn[1];
            w = (int)arg2.getContentAsScalarIndex(true);
            bool wValid = (w == 0 || w == 1);
            if (!wValid) {
                Error(_W("Wrong value for #2 argument."));
            }
        }
        if (argIn.size() > 2) {
            ArrayOf arg3 = argIn[2];
            d = (int)arg3.getContentAsScalarIndex(true);
            if (d <= 0) {
                Error(_W("Wrong value for #3 argument."));
            }
        }
        bool needToOverload = false;
        ArrayOf res = Variance(A, w, d, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "var");
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
