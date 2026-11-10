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
#include "Dimensions.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
// Internal helpers shared by the kernel implementation units
// (GpuElementwise, GpuMatMul, GpuReductions). Not part of the module's
// public builtin surface.
//=============================================================================
namespace Nelson {
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
// a 1-D grid (Y = Z = 1) wider than the device limit is folded into X * Y
void
GpuDispatchKernel(WGPUComputePipeline pipeline, const std::vector<WGPUBuffer>& storageBuffers,
    const uint32_t* params, size_t paramCount, uint32_t workgroupCountX, uint32_t workgroupCountY,
    uint32_t workgroupCountZ = 1);
//=============================================================================
// one dispatch of a sequence: its pipeline, uniform parameters and grid
struct GpuDispatchStep
{
    WGPUComputePipeline pipeline;
    std::vector<uint32_t> params;
    uint32_t workgroupCountX;
    uint32_t workgroupCountY;
    uint32_t workgroupCountZ;
};
//=============================================================================
// steps sharing the same storage buffers, dispatched in order in one submit (pipelines with
// the same binding layout may alternate)
void
GpuDispatchSteps(
    const std::vector<WGPUBuffer>& storageBuffers, const std::vector<GpuDispatchStep>& steps);
//=============================================================================
// same kernel and buffers dispatched once per parameter set, in order, in one submit
void
GpuDispatchKernelSequence(WGPUComputePipeline pipeline,
    const std::vector<WGPUBuffer>& storageBuffers,
    const std::vector<std::vector<uint32_t>>& paramSets, uint32_t workgroupCountX,
    uint32_t workgroupCountY, uint32_t workgroupCountZ = 1);
//=============================================================================
/**
 * Create a device storage buffer holding the given u32 indices (uploaded).
 * Caller owns the returned buffer and must wgpuBufferRelease() it after the
 * consuming kernel has completed (see GpuContext::waitIdle()).
 */
WGPUBuffer
GpuCreateIndexBuffer(const std::vector<uint32_t>& indices);
//=============================================================================
// operand layout of a binary elementwise op: scalar, same size or implicit expansion
struct GpuBinaryGeometry
{
    Dimensions outputDims;
    // true when an operand is repeated along some dimension (not a scalar)
    bool expand = false;
    // uniform parameters matching GpuBinaryParamsWgsl(expand)
    std::vector<uint32_t> params;
};
//=============================================================================
// output size and operand steps of a op b; raises an error when the sizes do not match
GpuBinaryGeometry
GpuComputeBinaryGeometry(const Dimensions& a, const Dimensions& b);
//=============================================================================
// WGSL `Params` struct and `operandIndex(i) -> vec2<u32>` (positions in a and b)
std::string
GpuBinaryParamsWgsl(bool expand);
//=============================================================================
// compile (once per layout) and dispatch one thread per output element
void
GpuRunBinaryKernel(const std::string& opKey, const std::string& source,
    const GpuBinaryGeometry& geometry, const std::vector<WGPUBuffer>& buffers);
//=============================================================================
/** Build the WGSL source for a binary elementwise kernel (a,b -> c). */
std::string
GpuBuildBinaryWgsl(const std::string& expr, bool expand);
//=============================================================================
/** Build the WGSL source for a unary elementwise kernel (a -> c). */
std::string
GpuBuildUnaryWgsl(const std::string& expr);
//=============================================================================
// one variable of the per-lane state of a reduction or a scan
struct GpuLaneVar
{
    std::string name;
    std::string type;
};
//=============================================================================
// WGSL pieces of a lane reduction/scan; the input is `a`, element `e` is at lane position `k`
struct GpuLaneSpec
{
    std::vector<GpuLaneVar> state;
    // defines locals from element `e` / position `k`
    std::string load;
    // defines the same locals from another partial state `other_<name>`
    std::string mergeLoad;
    // sets the state from the locals
    std::string init;
    // folds the locals into the state (must be associative to run in parallel)
    std::string update;
    // reduction: writes the result of lane `o`; scan: writes element `e`
    std::string store;
    // output buffer names, bound after `a`
    std::vector<std::string> outputs;
    // extra u32 fields of `params` after {outCount, redLen, before}
    std::vector<std::string> extraParams;
    // false for a stepwise saturating scan, which cannot be split
    bool associative = true;
    // WGSL functions placed before the kernel (e.g. the accurate math library)
    std::string helpers;
};
//=============================================================================
// reduce every lane of `input` with `spec` (serial per lane or one workgroup per lane)
void
GpuRunLaneReduce(const std::string& opKey, const GpuLaneSpec& spec, const GpuLaneGeometry& lane,
    WGPUBuffer input, const std::vector<WGPUBuffer>& outputs,
    const std::vector<uint32_t>& extraParams = { });
//=============================================================================
// true when a non-associative scan of this lane is too long for one GPU thread
bool
GpuLaneTooLongForSerialScan(const GpuLaneGeometry& lane);
//=============================================================================
// inclusive scan of every lane of `input` into `outputs` with `spec`
void
GpuRunLaneScan(const std::string& opKey, const GpuLaneSpec& spec, const GpuLaneGeometry& lane,
    WGPUBuffer input, const std::vector<WGPUBuffer>& outputs,
    const std::vector<uint32_t>& extraParams = {});
//=============================================================================
} // namespace Nelson
//=============================================================================
