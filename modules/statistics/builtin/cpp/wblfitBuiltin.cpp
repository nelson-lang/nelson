//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "wblfitBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsDistributions.hpp"
//=============================================================================
namespace Nelson::StatisticsGateway {
//=============================================================================
ArrayOfVector
wblfitBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 5);
    nargoutcheck(nLhs, 0, 2);
    WeibullFitResult result = WeibullFit(argIn);
    ArrayOfVector retval;
    retval << result.parameterHat;
    if (nLhs > 1) {
        retval << result.parameterCI;
    }
    return retval;
}
//=============================================================================
} // namespace Nelson::StatisticsGateway
//=============================================================================
