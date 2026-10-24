//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "clusterBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsClustering.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::clusterBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    retval << Cluster(argIn);
    return retval;
}
//=============================================================================
