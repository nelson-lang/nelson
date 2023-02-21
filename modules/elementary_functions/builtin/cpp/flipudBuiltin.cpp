//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "flipudBuiltin.hpp"
#include "Flip.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "TruncateFunctions.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::flipudBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "flipud", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        ArrayOf res = Flipud(argIn[0], needToOverload);
        if (needToOverload) {
            ArrayOfVector args = argIn;
            args.push_back(ArrayOf::doubleConstructor(1.));
            Context* context = eval->getContext();
            if (context != nullptr) {
                FunctionDef* funcDef = nullptr;
                if (context->lookupFunction("flip", funcDef)) {
                    if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                        || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                        return funcDef->evaluateFunction(eval, args, nLhs);
                    }
                }
            }
        } else {
            retval << res;
        }
    }
    return retval;
}
//=============================================================================
