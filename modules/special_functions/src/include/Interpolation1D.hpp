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
#include <functional>
#include <string>
#include "ArrayOf.hpp"
#include "nlsSpecial_functions_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Evaluates the not-a-knot spline through (x, y) at q (all column vectors).
 * The caller provides the language 'spline' function.
 */
using Interpolation1DSpline
    = std::function<ArrayOf(const ArrayOf& x, const ArrayOf& y, const ArrayOf& q)>;
//=============================================================================
/**
 * Native interp1 for dense double/single inputs.
 * X == nullptr selects the implicit sample points 1:n.
 * method: linear, nearest, previous, next, pchip, spline, cubic or makima.
 * extrapolationMode: default, extrap or constant (extrapolationValue is used
 * only with constant).
 * Returns false, without side effects, when the inputs are outside the native
 * scope (other classes, invalid or unusual arguments): the caller then uses
 * the generic implementation, which also raises the errors and warnings.
 */
NLSSPECIAL_FUNCTIONS_IMPEXP bool
Interpolation1D(const ArrayOf* X, const ArrayOf& V, const ArrayOf& XQ, const std::wstring& method,
    const std::wstring& extrapolationMode, const ArrayOf& extrapolationValue,
    const Interpolation1DSpline& spline, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
