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
//=============================================================================
NLSOPERATORS_IMPEXP ArrayOf
Subtraction(const ArrayOf& A, const ArrayOf& B);
//=============================================================================
/**
 * Fast element-wise difference of a complex operand and a real operand.
 * Subtracts on the real part only ((a + bi) - c = (a - c) + bi, c - (a + bi) =
 * (c - a) - bi) instead of promoting the real operand to complex. The result is
 * bit-identical for every value (no cross term), so it runs for dense operands of
 * matching float width with equal shapes or one scalar, and returns false otherwise.
 * Operand order is preserved.
 */
NLSOPERATORS_IMPEXP bool
dotMinusComplexRealScaling(const ArrayOf& A, const ArrayOf& B, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
