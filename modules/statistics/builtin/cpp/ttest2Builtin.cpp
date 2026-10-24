//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ttest2Builtin.hpp"
#include "StatisticsDistributions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::ttest2Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 10);
    nargoutcheck(nLhs, 0, 4);
    TTestResult result = TTest2(argIn);
    ArrayOfVector retval;
    retval << result.h;
    if (nLhs > 1) {
        retval << result.p;
    }
    if (nLhs > 2) {
        retval << result.ci;
    }
    if (nLhs > 3) {
        retval << result.stats;
    }
    return retval;
}
//=============================================================================
