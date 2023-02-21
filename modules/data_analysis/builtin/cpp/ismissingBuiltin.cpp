//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ismissingBuiltin.hpp"
#include "Error.hpp"
#include "IsMissing.hpp"
#include "OverloadFunction.hpp"
#include "CheckerHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DataAnalysisGateway::ismissingBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "ismissing", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "ismissing", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        bool needToOverload = false;
        retval << isMissing(argIn[0], needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "ismissing");
        }
    }
    return retval;
}
//=============================================================================
