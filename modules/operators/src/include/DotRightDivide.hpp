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
#include "nlsOperators_exports.h"
//=============================================================================
namespace Nelson {
NLSOPERATORS_IMPEXP ArrayOf
DotRightDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
/**
 * Fast element-wise division of a complex numerator by a real divisor.
 * Scales each element ((a + bi) / c = (a / c) + (b / c)i) instead of promoting the
 * divisor to complex. Runs only when the result is bit-identical to that path (dense
 * operands, matching float width, equal shapes or one scalar, finite numerator, finite
 * nonzero divisor) and returns false otherwise.
 */
NLSOPERATORS_IMPEXP bool
dotRightDivideComplexRealScaling(const ArrayOf& numerator, const ArrayOf& divisor, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
