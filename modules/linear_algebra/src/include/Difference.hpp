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
#include <functional>
#include "ArrayOf.hpp"
#include "nlsLinear_algebra_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Element-wise subtraction used between two consecutive slices. The caller
 * provides the language '-' operator so that class rules, saturation and
 * overloads stay those of the interpreter.
 */
using DifferenceSubtraction = std::function<ArrayOf(const ArrayOf&, const ArrayOf&)>;
//=============================================================================
/**
 * Differences of order 'order' (positive integer) of x.
 * dim is one-based; dim == 0 selects the first non-singleton dimension and
 * continues along the next dimensions when the order exceeds its length.
 */
NLSLINEAR_ALGEBRA_IMPEXP ArrayOf
Difference(const ArrayOf& x, double order, indexType dim, const DifferenceSubtraction& subtract);
//=============================================================================
} // namespace Nelson
//=============================================================================
