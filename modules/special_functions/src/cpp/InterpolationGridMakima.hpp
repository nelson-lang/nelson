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
//=============================================================================
namespace Nelson {
//=============================================================================
// Tensor-product modified Akima interpolation on a grid (real double data).
// grids: strictly increasing column vectors, one per dimension of V (the
// remaining dimensions of V are pages). queries: one array per dimension,
// paired element by element when sameSize, otherwise spanning a grid.
// Outside the grid: constant when useConstant, otherwise extrapolation.
//=============================================================================
ArrayOf
makimaOnGrid(const std::vector<ArrayOf>& grids, const ArrayOf& V,
    const std::vector<ArrayOf>& queries, bool sameSize, bool useConstant, double constant);
//=============================================================================
} // namespace Nelson
//=============================================================================
