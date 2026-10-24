//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "kstestBuiltin.hpp"
#include "StatisticsDistributions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::kstestBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 7);
    nargoutcheck(nLhs, 0, 4);
    KSTestResult result = KSTest(eval, argIn);
    ArrayOfVector retval;
    retval << result.h;
    if (nLhs > 1) {
        retval << result.p;
    }
    if (nLhs > 2) {
        retval << result.statistic;
    }
    if (nLhs > 3) {
        retval << result.criticalValue;
    }
    return retval;
}
//=============================================================================
