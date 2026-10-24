//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "raylrndBuiltin.hpp"
#include "StatisticsDistributions.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::raylrndBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 18);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << RayleighRnd(argIn);
    return retval;
}
//=============================================================================
