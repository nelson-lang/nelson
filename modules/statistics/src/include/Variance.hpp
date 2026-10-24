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
#include <vector>
#include "ArrayOf.hpp"
#include "nlsStatistics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSSTATISTICS_IMPEXP
ArrayOf
Variance(const ArrayOf& A, int w, int dim, bool omitNaN, bool& needToOverload);
//=============================================================================
// Variance over a vector of operating dimensions (e.g. var(A, w, [1 2])).
// The variance is computed over all elements spanning the listed dimensions,
// so they are gathered together first; the result has size 1 on each of them.
NLSSTATISTICS_IMPEXP
ArrayOf
Variance(const ArrayOf& A, int w, const std::vector<indexType>& dims, bool omitNaN,
    bool& needToOverload);
//=============================================================================
// Weighted variance: 'weights' is a real vector whose length matches the size
// of the operating dimension. Normalization is always by sum(weights).
NLSSTATISTICS_IMPEXP
ArrayOf
Variance(const ArrayOf& A, const ArrayOf& weights, int dim, bool omitNaN, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
