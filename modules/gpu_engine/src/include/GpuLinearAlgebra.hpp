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
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
// each function returns false when the call is outside what the device covers
namespace Nelson {
//=============================================================================
// true for a real single gpuArray (the matrices the dense solvers take)
NLSGPU_ENGINE_IMPEXP bool
GpuIsRealSingle(GpuArrayObject* a);
//=============================================================================
// lu with partial pivoting: outputs as [Y], [L,U], [L,U,P] or [L,U,p] when vectorForm
NLSGPU_ENGINE_IMPEXP bool
GpuLu(GpuArrayObject* a, int nLhs, bool vectorForm, ArrayOfVector& outputs);
//=============================================================================
NLSGPU_ENGINE_IMPEXP bool
GpuDet(GpuArrayObject* a, ArrayOf& result);
//=============================================================================
// a \ b for a square a
NLSGPU_ENGINE_IMPEXP bool
GpuSolveSquare(GpuArrayObject* a, GpuArrayObject* b, ArrayOf& result);
//=============================================================================
NLSGPU_ENGINE_IMPEXP bool
GpuInv(GpuArrayObject* a, ArrayOf& result);
//=============================================================================
// Cholesky factor of the symmetric matrix given by the upper triangle: R or, when lower, R'
NLSGPU_ENGINE_IMPEXP bool
GpuChol(GpuArrayObject* a, bool lower, ArrayOf& result);
//=============================================================================
// one-sided Jacobi on a real m-by-n (m >= n) matrix: w = a * v with orthogonal columns; false
// when it does not converge or the input is outside what the device covers
NLSGPU_ENGINE_IMPEXP bool
GpuJacobiSvd(GpuArrayObject* a, ArrayOf& w, ArrayOf& v);
//=============================================================================
// Householder qr: [R] or [Q, R], economy size when asked
NLSGPU_ENGINE_IMPEXP bool
GpuQr(GpuArrayObject* a, int nLhs, bool economy, ArrayOfVector& outputs);
//=============================================================================
} // namespace Nelson
//=============================================================================
