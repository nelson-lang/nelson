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
#include <cstdint>
#include <string>
#include <vector>
#include "GpuWebGpu.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
// Internal helpers shared by the kernel implementation units
// (GpuElementwise, GpuMatMul, GpuReductions). Not part of the module's
// public builtin surface.
//=============================================================================
namespace Nelson {
//=============================================================================
class Dimensions;
//=============================================================================
/**
 * Column-major "lane" decomposition for operations along one dimension.
 * A lane groups the `redLen` elements reduced/scanned/sorted together; there
 * are `laneCount` lanes. For lane l, the first element is at
 * start = (l % before) + (l / before) * (before * redLen), step = before.
 */
struct GpuLaneGeometry
{
    bool identity = false; // the selected dimension is a singleton
    int dimension = 0; // resolved 1-based dimension
    uint32_t laneCount = 0;
    uint32_t redLen = 0;
    uint32_t before = 0;
};
//=============================================================================
/**
 * Resolve the lane geometry for reducing/scanning/sorting `dims` along `dim`
 * (dim <= 0 selects the first non-singleton dimension).
 */
GpuLaneGeometry
GpuComputeLaneGeometry(const Dimensions& dims, int dim);
//=============================================================================
/**
 * Bind the given storage buffers (bindings 0..N-1) plus a uniform parameter
 * buffer (binding N), then dispatch the compute pipeline.
 */
void
GpuDispatchKernel(WGPUComputePipeline pipeline, const std::vector<WGPUBuffer>& storageBuffers,
    const uint32_t* params, size_t paramCount, uint32_t workgroupCountX, uint32_t workgroupCountY);
//=============================================================================
/**
 * Create a device storage buffer holding the given u32 indices (uploaded).
 * Caller owns the returned buffer and must wgpuBufferRelease() it after the
 * consuming kernel has completed (see GpuContext::waitIdle()).
 */
WGPUBuffer
GpuCreateIndexBuffer(const std::vector<uint32_t>& indices);
//=============================================================================
/** Build the WGSL source for a binary elementwise kernel (a,b -> c). */
std::string
GpuBuildBinaryWgsl(const std::string& expr);
//=============================================================================
/** Build the WGSL source for a unary elementwise kernel (a -> c). */
std::string
GpuBuildUnaryWgsl(const std::string& expr);
//=============================================================================
/**
 * Build the WGSL source for an N-D strided reduction kernel (a -> c).
 * Reduction geometry is passed as {outCount, redLen, before}: for output o,
 * start = (o % before) + (o / before) * (before * redLen), step = before.
 * `initExpr` maps the first element into the accumulator (e.g. "a[start]" for
 * sum, "f32(a[start] != 0.0)" for any/all).
 */
std::string
GpuBuildReduceWgsl(
    const std::string& initExpr, const std::string& combineExpr, const std::string& finishExpr);
//=============================================================================
/**
 * Build the WGSL source for an arg-reduction kernel (a -> values, indices).
 * `compareExpr` (using `v` and `acc`) selects a new best, e.g. "v > acc".
 * Indices are 1-based positions along the reduced dimension.
 */
std::string
GpuBuildArgReduceWgsl(const std::string& compareExpr);
//=============================================================================
} // namespace Nelson
//=============================================================================
