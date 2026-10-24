//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "logncdfBuiltin.hpp"
#include "StatisticsDistributions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::logncdfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 6);
    nargoutcheck(nLhs, 0, 3);
    NormalBounds result = LognormalCdf(argIn);
    ArrayOfVector retval;
    retval << result.value;
    if (nLhs > 1) {
        retval << result.lower;
    }
    if (nLhs > 2) {
        retval << result.upper;
    }
    return retval;
}
//=============================================================================
