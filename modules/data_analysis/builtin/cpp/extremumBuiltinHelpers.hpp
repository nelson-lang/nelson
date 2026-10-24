//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson::DataAnalysisGateway {
//=============================================================================
// C = f(A, B)
using extremum_binary_function = ArrayOf (*)(bool, const ArrayOf&, const ArrayOf&, bool&);
// [M, I] = f(A)
using extremum_unary_function = ArrayOfVector (*)(bool, const ArrayOf&, int, bool&);
// [M, I] = f(A, [], dim)
using extremum_dimension_function = ArrayOfVector (*)(bool, const ArrayOf&, indexType, int, bool&);
//=============================================================================
struct ExtremumFunctions
{
    extremum_binary_function binary;
    extremum_unary_function unary;
    extremum_dimension_function alongDimension;
};
//=============================================================================
// shared argument handling of max and min:
//   M = f(A), [M, I] = f(A, [], dim | vecdim | 'all', nanflag, 'linear'), C = f(A, B, nanflag)
ArrayOfVector
extremumBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, const ExtremumFunctions& functions);
//=============================================================================
} // namespace Nelson::DataAnalysisGateway
//=============================================================================
