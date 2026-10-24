//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "varBuiltin.hpp"
#include "OverloadRequired.hpp"
#include "VarianceOptions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::varBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 2);
    VarianceOptions options = ParseVarianceOptions(argIn);
    bool sparseOutput = argIn[0].isSparse();
    ArrayOf x = PrepareVarianceInput(argIn[0]);
    bool needToOverload = false;
    ArrayOf variance = VarianceWithOptions(x, options, needToOverload);
    if (needToOverload) {
        OverloadRequired("var");
    }
    if (sparseOutput) {
        variance.makeSparse();
    }
    retval << variance;
    if (nLhs > 1) {
        ArrayOf meanValue = VarianceMean(x, options, needToOverload);
        if (needToOverload) {
            OverloadRequired("var");
        }
        if (sparseOutput) {
            meanValue.makeSparse();
        }
        retval << meanValue;
    }
    return retval;
}
//=============================================================================
