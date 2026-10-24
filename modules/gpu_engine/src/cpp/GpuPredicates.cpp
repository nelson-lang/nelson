//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Elementwise NaN/Inf predicates (logical result). An integer array has no
// NaN/Inf, so the answer is a constant logical array; complex inputs test both
// components via the GpuComplex* kernels.
//=============================================================================
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
constantLogicalGpu(const Dimensions& dims, logical value);
static bool
isIntegerInput(GpuArrayObject* a);
//=============================================================================
ArrayOf
GpuIsNaN(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexIsNaN(a);
    }
    if (isIntegerInput(a)) {
        return constantLogicalGpu(a->dimensions(), 0);
    }
    // Inspect IEEE-754 bits so backend fast-math cannot fold NaN comparisons.
    return GpuUnaryElementwise(
        a, "gpuArray:isnan", "f32((bitcast<u32>(ai) & 0x7fffffffu) > 0x7f800000u)", "logical");
}
//=============================================================================
ArrayOf
GpuIsInf(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexIsInf(a);
    }
    if (isIntegerInput(a)) {
        return constantLogicalGpu(a->dimensions(), 0);
    }
    return GpuUnaryElementwise(
        a, "gpuArray:isinf", "f32((bitcast<u32>(ai) & 0x7fffffffu) == 0x7f800000u)", "logical");
}
//=============================================================================
ArrayOf
GpuIsFinite(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexIsFinite(a);
    }
    if (isIntegerInput(a)) {
        return constantLogicalGpu(a->dimensions(), 1);
    }
    return GpuUnaryElementwise(
        a, "gpuArray:isfinite", "f32((bitcast<u32>(ai) & 0x7fffffffu) < 0x7f800000u)", "logical");
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
constantLogicalGpu(const Dimensions& dims, logical value)
{
    indexType n = dims.elementCount();
    logical* buffer = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, n);
    for (indexType k = 0; k < n; ++k) {
        buffer[k] = value;
    }
    return CreateGpuArray(ArrayOf(NLS_LOGICAL, dims, buffer));
}
//=============================================================================
static bool
isIntegerInput(GpuArrayObject* a)
{
    return a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
