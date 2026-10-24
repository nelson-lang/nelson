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
#include "nlsData_analysis_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class MovingWindowOperation
{
    Median,
    Mad,
    Sum,
    Mean,
    Min,
    Max
};
//=============================================================================
// true when name is one of median, mad, sum, mean, min, max.
NLSDATA_ANALYSIS_IMPEXP bool
MovingWindowOperationFromName(const std::wstring& name, MovingWindowOperation& operation);
//=============================================================================
// Sliding statistic of every column of a real 2-D numeric array x (n rows).
// Row r of the result, for firstRow <= r <= lastRow (1-based), holds for each
// column the statistic of the rows max(1, r - before) .. min(n, r + after).
// The result is double, as the mov* functions return it.
// omitNaN drops the NaN values of a window; otherwise a NaN anywhere in the
// window gives NaN (sum and mean propagate it, min and max report it).
// The statistics follow the reductions of the language applied to a window:
//   sum accumulates sequentially in the class of x (double for integers),
//   mean accumulates in double and divides by the count,
//   median rounds the median of two integer values half away from zero and
//   keeps single precision, mad is the median absolute deviation with the
//   saturated integer arithmetic of the language.
NLSDATA_ANALYSIS_IMPEXP ArrayOf
MovingWindow(const ArrayOf& x, indexType before, indexType after, indexType firstRow,
    indexType lastRow, bool omitNaN, MovingWindowOperation operation, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
