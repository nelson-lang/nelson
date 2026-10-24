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
// Options shared by var and std: var(A, w, dim | vecdim | 'all', nanflag).
struct VarianceOptions
{
    int weight = 0;
    int dim = -1;
    bool omitNaN = false;
    bool allElements = false;
    bool hasWeightVector = false;
    bool useVecDim = false;
    ArrayOf weightVector;
    std::vector<indexType> vecDims;
};
//=============================================================================
NLSSTATISTICS_IMPEXP VarianceOptions
ParseVarianceOptions(const ArrayOfVector& argIn);
//=============================================================================
// Dense copy of the input data; logical, char and integer data become double.
NLSSTATISTICS_IMPEXP ArrayOf
PrepareVarianceInput(const ArrayOf& x);
//=============================================================================
NLSSTATISTICS_IMPEXP ArrayOf
VarianceWithOptions(const ArrayOf& x, const VarianceOptions& options, bool& needToOverload);
//=============================================================================
// Mean used by the variance: weighted mean when a weight vector is given,
// computed over the same operating dimensions and with the same nanflag.
NLSSTATISTICS_IMPEXP ArrayOf
VarianceMean(const ArrayOf& x, const VarianceOptions& options, bool& needToOverload);
//=============================================================================
// Gathers the dimensions listed in 'dims' to the front of A and collapses them
// into the first dimension of 'collapsed' (reduced count x kept count).
// 'outDim' receives the size of a reduction result. Returns false when at most
// one effective dimension is reduced: 'singleDim' (one-based) is then set.
bool
CollapseOperatingDimensions(const ArrayOf& A, const std::vector<indexType>& dims,
    ArrayOf& collapsed, Dimensions& outDim, int& singleDim, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
