//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "minBuiltin.hpp"
#include "Minimum.hpp"
#include "extremumBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// M = min(A)
// [M, I] = min(A, [], dim | vecdim | 'all', nanflag, 'linear')
// C = min(A, B, nanflag)
ArrayOfVector
Nelson::DataAnalysisGateway::minBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    static const ExtremumFunctions functions = {
        static_cast<extremum_binary_function>(Minimum),
        static_cast<extremum_unary_function>(Minimum),
        static_cast<extremum_dimension_function>(Minimum),
    };
    return extremumBuiltin(eval, nLhs, argIn, "min", functions);
}
//=============================================================================
