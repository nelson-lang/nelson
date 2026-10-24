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
elementWiseMultiplication(const ArrayOf& A, const ArrayOf& B, bool& needToOverload);
//=============================================================================
/**
 * Fast element-wise product of a complex operand by a real operand.
 * Scales each element ((a + bi) * c = (a * c) + (b * c)i) instead of promoting the
 * real operand to complex. Runs only when the result is bit-identical to that path
 * (dense operands, matching float width, equal shapes or one scalar, all values
 * finite) and returns false otherwise. Either operand order is accepted.
 */
NLSOPERATORS_IMPEXP bool
dotTimesComplexRealScaling(const ArrayOf& A, const ArrayOf& B, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
