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
#include <limits>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsData_analysis_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class LocalExtremaFlatSelection
{
    Center,
    First,
    Last,
    All
};
//=============================================================================
struct LocalExtremaOptions
{
    bool isMaximum = true;
    LocalExtremaFlatSelection flatSelection = LocalExtremaFlatSelection::Center;
    // minimum prominence in the class of the input (0: no filter).
    double minProminence = 0;
    // minimum separation in sample point units (0: no filter).
    double minSeparation = 0;
    // most prominent extrema kept (infinite: no limit).
    double maxNumExtrema = std::numeric_limits<double>::infinity();
    // sample points along the operating dimension (empty: 1, 2, 3, ...).
    std::vector<double> samplePoints;
    // prominence window [x - before, x + after] around each extremum; the
    // upper bound is excluded when rightOpen is true (scalar window length).
    bool hasWindow = false;
    double windowBefore = 0;
    double windowAfter = 0;
    bool windowRightOpen = false;
    bool wantProminence = false;
};
//=============================================================================
// Local maxima (or minima) of the real array A along the dimension dim
// (1-based). An extremum is an element, or a flat run of equal elements, that
// is strictly greater (smaller) than its neighbors; NaN values are ignored,
// endpoints are never extrema and +Inf (-Inf for minima) always is one.
// tf is logical, prominence has the class of A (unsigned for integers) and is
// only computed when options.wantProminence is true.
NLSDATA_ANALYSIS_IMPEXP void
LocalExtrema(const ArrayOf& A, indexType dim, const LocalExtremaOptions& options, ArrayOf& tf,
    ArrayOf& prominence);
//=============================================================================
} // namespace Nelson
//=============================================================================
