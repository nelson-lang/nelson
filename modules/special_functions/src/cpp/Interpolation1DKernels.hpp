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
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Real double kernels of interp1. x is strictly increasing with n points.
// useConstant selects the constant value outside [x(1), x(n)], otherwise
// cubic returns NaN outside and makima extrapolates.
//=============================================================================
bool
isUniformGrid(const double* x, indexType n);
//=============================================================================
void
cubicConvolution1D(const double* x, const double* y, indexType n, const double* q, indexType nq,
    bool useConstant, double constant, double* out);
//=============================================================================
void
makima1D(const double* x, const double* y, indexType n, const double* q, indexType nq,
    bool useConstant, double constant, double* out);
//=============================================================================
std::vector<double>
makimaSlopes(const double* x, const double* y, indexType n);
//=============================================================================
// piecewise cubic Hermite value at xk (the boundary pieces extrapolate)
double
makimaHermite(const double* x, const double* y, const double* d, indexType n, double xk);
//=============================================================================
} // namespace Nelson
//=============================================================================
