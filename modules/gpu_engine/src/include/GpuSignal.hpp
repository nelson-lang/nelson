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
#include <vector>
#include "ArrayOf.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
// each function returns false when the call is outside what the device covers
namespace Nelson {
//=============================================================================
NLSGPU_ENGINE_IMPEXP bool
GpuPolyval(GpuArrayObject* p, GpuArrayObject* x, ArrayOf& result);
//=============================================================================
// shape is "full", "same" or "valid"; real vectors
NLSGPU_ENGINE_IMPEXP bool
GpuConv(GpuArrayObject* u, GpuArrayObject* v, const std::string& shape, ArrayOf& result);
//=============================================================================
// 2-D convolution of real matrices: shape "full", "same" or "valid"
NLSGPU_ENGINE_IMPEXP bool
GpuConv2(GpuArrayObject* a, GpuArrayObject* b, const std::string& shape, ArrayOf& result);
//=============================================================================
// v sampled on one ascending grid vector per array dimension (2 or 3), read at queries of one
// size given per dimension; linear or nearest, NaN or `fill` outside
NLSGPU_ENGINE_IMPEXP bool
GpuInterpGrid(const std::vector<GpuArrayObject*>& grids, GpuArrayObject* v,
    const std::vector<GpuArrayObject*>& queries, bool nearest, bool useFill, float fill,
    ArrayOf& result);
//=============================================================================
// coefficients on the host, x filtered along dim (dim <= 0: first dimension not of length 1)
NLSGPU_ENGINE_IMPEXP bool
GpuFilter(const std::vector<float>& b, const std::vector<float>& a, GpuArrayObject* x, int dim,
    ArrayOf& result);
//=============================================================================
// method: linear, nearest, previous, next; extrapMode 0 NaN, 1 fillValue, 2 extrapolate
NLSGPU_ENGINE_IMPEXP bool
GpuInterp1(GpuArrayObject* x, GpuArrayObject* v, GpuArrayObject* q, const std::string& method,
    int extrapMode, float fillValue, ArrayOf& result);
//=============================================================================
// trapezoidal integral (or its running value) with a constant spacing
NLSGPU_ENGINE_IMPEXP bool
GpuTrapz(GpuArrayObject* y, float spacing, int dim, bool cumulative, ArrayOf& result);
//=============================================================================
// moving sum, mean, prod, max, min, var or std over [t - before, t + after] along dim
NLSGPU_ENGINE_IMPEXP bool
GpuMoving(GpuArrayObject* x, const std::string& op, uint32_t before, uint32_t after, int dim,
    bool omitNan, bool normalizeByN, ArrayOf& result);
//=============================================================================
} // namespace Nelson
//=============================================================================
