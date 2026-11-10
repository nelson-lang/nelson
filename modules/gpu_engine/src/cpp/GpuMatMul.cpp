//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "GpuKernels.hpp"
#include "GpuComplexKernels.hpp"
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
// Workgroup z computes the k range of one chunk into its own slice of c.
static const char* MTIMES_TILED_WGSL = R"WGSL(
struct Params { m : u32, k : u32, n : u32, kChunk : u32, rowOffset : u32, colOffset : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> b : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
const TILE : u32 = 16u;
var<workgroup> tileA : array<f32, 256u>;
var<workgroup> tileB : array<f32, 256u>;
@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) gid : vec3<u32>,
        @builtin(local_invocation_id) lid : vec3<u32>,
        @builtin(workgroup_id) wid : vec3<u32>) {
    let row = gid.x + params.rowOffset;
    let col = gid.y + params.colOffset;
    let kBegin = wid.z * params.kChunk;
    let kEnd = min(params.k, kBegin + params.kChunk);
    var acc : f32 = 0.0;
    let numTiles = (kEnd - kBegin + TILE - 1u) / TILE;
    for (var t : u32 = 0u; t < numTiles; t = t + 1u) {
        let aCol = kBegin + t * TILE + lid.y;
        if (row < params.m && aCol < kEnd) {
            tileA[lid.y * TILE + lid.x] = a[aCol * params.m + row];
        } else {
            tileA[lid.y * TILE + lid.x] = 0.0;
        }
        let bRow = kBegin + t * TILE + lid.x;
        if (bRow < kEnd && col < params.n) {
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
        c[wid.z * params.m * params.n + col * params.m + row] = acc;
    }
}
)WGSL";
//=============================================================================
static const char* MTIMES_COMPLEX_WGSL = R"WGSL(
struct Params { m : u32, k : u32, n : u32, kChunk : u32, rowOffset : u32, colOffset : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> b : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(16, 16)
fn main(@builtin(global_invocation_id) gid : vec3<u32>,
        @builtin(workgroup_id) wid : vec3<u32>) {
    let row = gid.x + params.rowOffset;
    let col = gid.y + params.colOffset;
    if (row < params.m && col < params.n) {
        let kBegin = wid.z * params.kChunk;
        let kEnd = min(params.k, kBegin + params.kChunk);
        var accRe : f32 = 0.0;
        var accIm : f32 = 0.0;
        for (var l : u32 = kBegin; l < kEnd; l = l + 1u) {
            let ia = l * params.m + row;
            let ib = col * params.k + l;
            let ar = a[2u * ia]; let aim = a[2u * ia + 1u];
            let br = b[2u * ib]; let bim = b[2u * ib + 1u];
            accRe = accRe + ar * br - aim * bim;
            accIm = accIm + ar * bim + aim * br;
        }
        let ic = wid.z * params.m * params.n + col * params.m + row;
        c[2u * ic] = accRe;
        c[2u * ic + 1u] = accIm;
    }
}
)WGSL";
//=============================================================================
// split k when one thread would loop too long or the output alone cannot fill the GPU
static uint32_t
chooseChunkCount(uint32_t m, uint32_t k, uint32_t n)
{
    const uint32_t chunkLength = 4096u;
    uint64_t outputs = (uint64_t)m * (uint64_t)n;
    if (k <= chunkLength || (outputs >= 65536u && k <= 65536u)) {
        return 1u;
    }
    uint64_t chunks = ((uint64_t)k + chunkLength - 1u) / chunkLength;
    return (uint32_t)std::min<uint64_t>(chunks, 65535u);
}
//=============================================================================
static void
dispatchMtimes(const std::string& key, const char* source, GpuArrayObject* a, GpuArrayObject* b,
    GpuArrayObject* output, uint32_t m, uint32_t k, uint32_t n, bool complex)
{
    GpuContext* context = GpuContext::getInstance();
    WGPUComputePipeline pipeline = context->pipelineFor(key, source, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t chunks = chooseChunkCount(m, k, n);
    uint32_t kChunk = (k + chunks - 1u) / chunks;
    kChunk = std::max<uint32_t>(16u, (kChunk + 15u) & ~15u);
    chunks = (k + kChunk - 1u) / kChunk;

    GpuArrayObject* partial = nullptr;
    if (chunks > 1) {
        partial = AllocateGpuArray(Dimensions((indexType)m * n, chunks), "single", complex);
    }
    WGPUBuffer target = partial ? partial->buffer() : output->buffer();
    try {
        // large outputs are covered in blocks that fit the grid limits
        uint32_t span = context->maxWorkgroupsPerDimension() * 16u;
        for (uint32_t colOffset = 0; colOffset < n; colOffset += std::min(span, n - colOffset)) {
            uint32_t cols = std::min(span, n - colOffset);
            for (uint32_t rowOffset = 0; rowOffset < m;
                rowOffset += std::min(span, m - rowOffset)) {
                uint32_t rows = std::min(span, m - rowOffset);
                uint32_t params[6] = { m, k, n, kChunk, rowOffset, colOffset };
                GpuDispatchKernel(pipeline, { a->buffer(), b->buffer(), target }, params, 6,
                    (rows + 15u) / 16u, (cols + 15u) / 16u, std::max(chunks, 1u));
            }
        }
        if (partial != nullptr) {
            // add the chunk results of every output element
            GpuLaneGeometry lane;
            lane.laneCount = m * n;
            lane.redLen = chunks;
            lane.before = m * n;
            GpuLaneSpec spec;
            if (complex) {
                spec.state = { { "accRe", "f32" }, { "accIm", "f32" } };
                spec.load = "    let vRe = a[2u * e];\n    let vIm = a[2u * e + 1u];\n";
                spec.mergeLoad = "    let vRe = other_accRe;\n    let vIm = other_accIm;\n";
                spec.init = "    accRe = vRe;\n    accIm = vIm;\n";
                spec.update = "    accRe = accRe + vRe;\n    accIm = accIm + vIm;\n";
                spec.store = "    c[2u * o] = accRe;\n    c[2u * o + 1u] = accIm;\n";
            } else {
                spec.state = { { "acc", "f32" } };
                spec.load = "    let v = a[e];\n";
                spec.mergeLoad = "    let v = other_acc;\n";
                spec.init = "    acc = v;\n";
                spec.update = "    acc = acc + v;\n";
                spec.store = "    c[o] = acc;\n";
            }
            spec.outputs = { "c" };
            GpuRunLaneReduce(complex ? "gpuArray:mtimes:csplit" : "gpuArray:mtimes:split", spec,
                lane, partial->buffer(), { output->buffer() });
        }
    } catch (...) {
        delete partial;
        throw;
    }
    delete partial;
}
//=============================================================================
static ArrayOf
complexMtimes(GpuArrayObject* a, GpuArrayObject* b, uint32_t m, uint32_t k, uint32_t n)
{
    GpuArrayObject* aTemp = a->isComplex() ? nullptr : GpuPromoteToComplex(a);
    GpuArrayObject* bTemp = b->isComplex() ? nullptr : GpuPromoteToComplex(b);
    GpuArrayObject* aC = aTemp ? aTemp : a;
    GpuArrayObject* bC = bTemp ? bTemp : b;
    ArrayOf result;
    try {
        GpuArrayObject* output = AllocateGpuArray(Dimensions(m, n), "single", true);
        result = ArrayOf::handle(output);
        if (m > 0 && n > 0) {
            dispatchMtimes(
                "gpuArray:mtimes_complex", MTIMES_COMPLEX_WGSL, aC, bC, output, m, k, n, true);
        }
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
    GpuArrayObject* output = AllocateGpuArray(Dimensions(m, n));
    ArrayOf result = ArrayOf::handle(output);
    if (m > 0 && n > 0) {
        dispatchMtimes("gpuArray:mtimes_tiled", MTIMES_TILED_WGSL, a, b, output, m, k, n, false);
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
