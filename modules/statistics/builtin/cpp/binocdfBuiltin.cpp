//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "binocdfBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsDistributions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::binocdfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 4);
    nargoutcheck(nLhs, 0, 1);
    return { BinomialCdf(argIn) };
}
//=============================================================================
