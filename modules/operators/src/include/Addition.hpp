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
Addition(const ArrayOf& A, const ArrayOf& B);
//=============================================================================
/**
 * Fast element-wise sum of a complex operand and a real operand.
 * Adds the real operand to the real part ((a + bi) + c = (a + c) + bi) instead of
 * promoting it to complex. The result is bit-identical for every value (no cross
 * term), so it runs for dense operands of matching float width with equal shapes or
 * one scalar, and returns false otherwise. Either operand order is accepted.
 */
NLSOPERATORS_IMPEXP bool
dotPlusComplexRealScaling(const ArrayOf& A, const ArrayOf& B, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
