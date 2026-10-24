//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GpuKernels.hpp"
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Tiled GEMM: C(MxN) = A(MxK) * B(KxN), all column-major.
static const char* MTIMES_TILED_WGSL = R"WGSL(
struct Params { m : u32, k : u32, n : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> b : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
const TILE : u32 = 16u;
var<workgroup> tileA : array<f32, 256u>;
var<workgroup> tileB : array<f32, 256u>;
@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) gid : vec3<u32>,
        @builtin(local_invocation_id) lid : vec3<u32>) {
    let row = gid.x;
    let col = gid.y;
    var acc : f32 = 0.0;
    let numTiles = (params.k + TILE - 1u) / TILE;
    for (var t : u32 = 0u; t < numTiles; t = t + 1u) {
        let aCol = t * TILE + lid.y;
        if (row < params.m && aCol < params.k) {
            tileA[lid.y * TILE + lid.x] = a[aCol * params.m + row];
        } else {
            tileA[lid.y * TILE + lid.x] = 0.0;
        }
        let bRow = t * TILE + lid.x;
        if (bRow < params.k && col < params.n) {
            tileB[lid.y * TILE + lid.x] = b[col * params.k + bRow];
        } else {
            tileB[lid.y * TILE + lid.x] = 0.0;
        }
        workgroupBarrier();
        for (var s : u32 = 0u; s < TILE; s = s + 1u) {
            acc = acc + tileA[s * TILE + lid.x] * tileB[lid.y * TILE + s];
        }
        workgroupBarrier();
    }
    if (row < params.m && col < params.n) {
        c[col * params.m + row] = acc;
    }
}
)WGSL";
// Simple (non-tiled) complex GEMM, interleaved (re, im), column-major.
static const char* MTIMES_COMPLEX_WGSL = R"WGSL(
struct Params { m : u32, k : u32, n : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> b : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let row = gid.x;
    let col = gid.y;
    if (row < params.m && col < params.n) {
        var accRe : f32 = 0.0;
        var accIm : f32 = 0.0;
        for (var l : u32 = 0u; l < params.k; l = l + 1u) {
            let ia = l * params.m + row;
            let ib = col * params.k + l;
            let ar = a[2u * ia]; let aim = a[2u * ia + 1u];
            let br = b[2u * ib]; let bim = b[2u * ib + 1u];
            accRe = accRe + ar * br - aim * bim;
            accIm = accIm + ar * bim + aim * br;
        }
        let ic = col * params.m + row;
        c[2u * ic] = accRe;
        c[2u * ic + 1u] = accIm;
    }
}
)WGSL";
//=============================================================================
static ArrayOf
complexMtimes(GpuArrayObject* a, GpuArrayObject* b, uint32_t m, uint32_t k, uint32_t n)
{
    GpuContext* context = GpuContext::getInstance();
    GpuArrayObject* aTemp = a->isComplex() ? nullptr : GpuPromoteToComplex(a);
    GpuArrayObject* bTemp = b->isComplex() ? nullptr : GpuPromoteToComplex(b);
    GpuArrayObject* aC = aTemp ? aTemp : a;
    GpuArrayObject* bC = bTemp ? bTemp : b;
    ArrayOf result;
    try {
        WGPUComputePipeline pipeline
            = context->pipelineFor("gpuArray:mtimes_complex", MTIMES_COMPLEX_WGSL, "main");
        if (pipeline == nullptr) {
            Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
        }
        GpuArrayObject* output = AllocateGpuArray(Dimensions(m, n), "single", true);
        if (m > 0 && n > 0) {
            uint32_t params[3] = { m, k, n };
            GpuDispatchKernel(pipeline, { aC->buffer(), bC->buffer(), output->buffer() }, params, 3,
                (m + 15u) / 16u, (n + 15u) / 16u);
        }
        result = ArrayOf::handle(output);
    } catch (...) {
        delete aTemp;
        delete bTemp;
        throw;
    }
    delete aTemp;
    delete bTemp;
    return result;
}
//=============================================================================
ArrayOf
GpuMtimes(GpuArrayObject* a, GpuArrayObject* b)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    // scalar * matrix routes to the elementwise kernel (handles complex and
    // int32, which the tiled f32 GEMM below cannot)
    if (a->dimensions().isScalar() || b->dimensions().isScalar()) {
        return GpuTimes(a, b);
    }
    // integer matrix * matrix is not defined (the scalar case was handled just
    // above); this matches the host, which requires a scalar operand
    if (a->isRawIntStorage() || b->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()
        || !GpuIntegerResultType(b, b).empty()) {
        Error(L"Nelson:mtimes:integerRequiresScalar",
            L"Integer matrix multiplication requires at least one scalar operand.");
    }
    Dimensions dimsA = a->dimensions();
    Dimensions dimsB = b->dimensions();
    if (!dimsA.is2D() || !dimsB.is2D()) {
        Error(L"Nelson:mtimes:inputsNot2D",
            L"Inputs must be 2-D, or at least one input must be scalar.");
    }
    uint32_t m = (uint32_t)dimsA.rows();
    uint32_t k = (uint32_t)dimsA.columns();
    uint32_t n = (uint32_t)dimsB.columns();
    if ((uint32_t)dimsB.rows() != k) {
        Error(L"Nelson:innerdim", L"Incorrect dimensions for matrix multiplication.");
    }
    if (a->isComplex() || b->isComplex()) {
        return complexMtimes(a, b, m, k, n);
    }
    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:mtimes_tiled", MTIMES_TILED_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    Dimensions outputDims(m, n);
    GpuArrayObject* output = AllocateGpuArray(outputDims);
    if (m > 0 && n > 0) {
        uint32_t params[3] = { m, k, n };
        uint32_t workgroupCountX = (m + 15u) / 16u;
        uint32_t workgroupCountY = (n + 15u) / 16u;
        GpuDispatchKernel(pipeline, { a->buffer(), b->buffer(), output->buffer() }, params, 3,
            workgroupCountX, workgroupCountY);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
