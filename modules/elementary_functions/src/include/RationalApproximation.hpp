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
#include "ArrayOf.hpp"
#include "nlsElementary_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Rational approximation of a real single/double array by truncated continued
// fraction expansions. When numericMode is true the result holds two arrays
// {numerator, denominator}; otherwise it holds a single character array with
// the continued fraction representation, one padded row per element.
NLSELEMENTARY_FUNCTIONS_IMPEXP ArrayOfVector
RationalApproximation(const ArrayOf& X, double tol, bool numericMode);
//=============================================================================
} // namespace Nelson
//=============================================================================
