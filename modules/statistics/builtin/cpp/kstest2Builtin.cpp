//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "kstest2Builtin.hpp"
#include "StatisticsDistributions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::kstest2Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 6);
    nargoutcheck(nLhs, 0, 3);
    KSTest2Result result = KSTest2(argIn);
    ArrayOfVector retval;
    retval << result.h;
    if (nLhs > 1) {
        retval << result.p;
    }
    if (nLhs > 2) {
        retval << result.statistic;
    }
    return retval;
}
//=============================================================================
