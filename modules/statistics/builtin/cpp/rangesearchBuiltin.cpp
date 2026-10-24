//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "rangesearchBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsDistances.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::rangesearchBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, -1);
    nargoutcheck(nLhs, 0, 2);
    return RangeSearch(argIn, nLhs);
}
//=============================================================================
