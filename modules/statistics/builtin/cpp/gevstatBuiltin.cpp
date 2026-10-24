//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gevstatBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsDistributions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::gevstatBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 2);
    GeneralizedExtremeValueStatResult result = GeneralizedExtremeValueStat(argIn);
    ArrayOfVector retval;
    retval << result.mean;
    if (nLhs > 1) {
        retval << result.variance;
    }
    return retval;
}
//=============================================================================
