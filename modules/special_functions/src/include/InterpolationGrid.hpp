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
#include "nlsSpecial_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Native interp2 and interp3 for dense double/single inputs.
 * X == nullptr (and Y, Z) selects the implicit grid 1:size(V, k) for each
 * dimension; otherwise X, Y (and Z) are grid vectors or full grids.
 * method: linear, nearest, cubic, spline or makima.
 * useConstant selects extrapolationValue outside the grid.
 * Returns false, without side effects, when the inputs are outside the native
 * scope (other classes, invalid or unusual arguments): the caller then uses
 * the generic implementation, which also raises the errors.
 */
NLSSPECIAL_FUNCTIONS_IMPEXP bool
Interpolation2D(const ArrayOf* X, const ArrayOf* Y, const ArrayOf& V, const ArrayOf& XQ,
    const ArrayOf& YQ, const std::wstring& method, bool useConstant,
    const ArrayOf& extrapolationValue, ArrayOf& result);
//=============================================================================
NLSSPECIAL_FUNCTIONS_IMPEXP bool
Interpolation3D(const ArrayOf* X, const ArrayOf* Y, const ArrayOf* Z, const ArrayOf& V,
    const ArrayOf& XQ, const ArrayOf& YQ, const ArrayOf& ZQ, const std::wstring& method,
    bool useConstant, const ArrayOf& extrapolationValue, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
