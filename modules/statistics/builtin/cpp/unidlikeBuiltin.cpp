//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "unidlikeBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StatisticsDistributions.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StatisticsGateway::unidlikeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    DiscreteUniformLikeResult result = DiscreteUniformLike(argIn);
    ArrayOfVector retval;
    retval << result.negativeLogLikelihood;
    if (nLhs > 1) {
        retval << result.covariance;
    }
    return retval;
}
//=============================================================================
