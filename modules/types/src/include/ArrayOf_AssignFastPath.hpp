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
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Fast path for indexed assignment `destination(index) = rightData` when both
// operands are dense arrays of the same plain numeric/char/logical class.
// Copies element-by-element with std::memcpy; `advance` is 1 for element-wise
// assignment and 0 to broadcast a scalar right-hand side.
// Returns false (leaving destination untouched) when the fast path does not
// apply, so the caller must fall back to the generic assignment routine.
bool
trySetDenseVectorSubsetDirect(ArrayOf& destination, const ArrayOf& rightData, constIndexPtr index,
    indexType length, int advance);
//=============================================================================
} // namespace Nelson
//=============================================================================
