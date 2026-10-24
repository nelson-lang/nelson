//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "maxBuiltin.hpp"
#include "Maximum.hpp"
#include "extremumBuiltinHelpers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// M = max(A)
// [M, I] = max(A, [], dim | vecdim | 'all', nanflag, 'linear')
// C = max(A, B, nanflag)
ArrayOfVector
Nelson::DataAnalysisGateway::maxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    static const ExtremumFunctions functions = {
        static_cast<extremum_binary_function>(Maximum),
        static_cast<extremum_unary_function>(Maximum),
        static_cast<extremum_dimension_function>(Maximum),
    };
    return extremumBuiltin(eval, nLhs, argIn, "max", functions);
}
//=============================================================================
