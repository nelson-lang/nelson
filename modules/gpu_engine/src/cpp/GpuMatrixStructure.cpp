//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GpuMatrixStructure.hpp"
#include "GpuElementwiseMath.hpp"
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
static WGPUComputePipeline
pipeline(const std::string& key, const std::string& source)
{
    WGPUComputePipeline p = GpuContext::getInstance()->pipelineFor(key, source.c_str(), "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    return p;
}
//=============================================================================
// one pass over a matrix: bandwidths (+1, 0 when none) and failed symmetry tests
static const char* STRUCTURE_WGSL = R"WGSL(
struct Params { rows : u32, cols : u32, complex : u32, pad : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> info : array<atomic<u32>>;
@group(0) @binding(2) var<uniform> params : Params;
fn re(i : u32) -> f32 { if (params.complex == 1u) { return a[2u * i]; } return a[i]; }
fn im(i : u32) -> f32 { if (params.complex == 1u) { return a[2u * i + 1u]; } return 0.0; }
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let k = gid.x;
    if (k >= params.rows * params.cols) { return; }
    let i = k % params.rows;
    let j = k / params.rows;
    let xr = re(k);
    let xi = im(k);
    if (xr != 0.0 || xi != 0.0) {
        if (i > j) { atomicMax(&info[0], i - j); }
        if (j > i) { atomicMax(&info[1], j - i); }
    }
    if (params.rows == params.cols) {
        let mirror = j + i * params.rows;
        let yr = re(mirror);
        let yi = im(mirror);
        if (xr != yr || xi != yi) { atomicOr(&info[2], 1u); }
        if (xr != -yr || xi != -yi) { atomicOr(&info[3], 1u); }
        if (xr != yr || xi != -yi) { atomicOr(&info[4], 1u); }
        if (xr != -yr || xi != yi) { atomicOr(&info[5], 1u); }
    }
}
)WGSL";
//=============================================================================
bool
GpuMatrixStructure(GpuArrayObject* a, GpuStructureInfo& info)
{
    Dimensions dims = a->dimensions();
    if (dims.length() > 2 || a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        return false;
    }
    info = GpuStructureInfo();
    info.rows = dims.rows();
    info.columns = dims.columns();
    if (a->elementCount() == 0) {
        info.symmetric = info.skewSymmetric = info.hermitian = info.skewHermitian
            = (info.rows == info.columns);
        return true;
    }
    GpuArrayObject* flags = AllocateGpuArray(Dimensions(1, 6), "uint32", false);
    ArrayOf owner = ArrayOf::handle(flags);
    uint32_t n = (uint32_t)a->elementCount();
    uint32_t params[4]
        = { (uint32_t)info.rows, (uint32_t)info.columns, a->isComplex() ? 1u : 0u, 0u };
    GpuDispatchKernel(pipeline("gpuArray:structure", STRUCTURE_WGSL),
        { a->buffer(), flags->buffer() }, params, 4, (n + 255u) / 256u, 1);
    ArrayOf host = GatherGpuArray(flags);
    const uint32_t* v = (const uint32_t*)host.dataPointer();
    info.lowerBandwidth = v[0];
    info.upperBandwidth = v[1];
    bool square = info.rows == info.columns;
    info.symmetric = square && v[2] == 0;
    info.skewSymmetric = square && v[3] == 0;
    info.hermitian = square && v[4] == 0;
    info.skewHermitian = square && v[5] == 0;
    return true;
}
//=============================================================================
// output element (i, j) of a structured matrix built from vectors u and w
static const char* STRUCTURED_WGSL = R"WGSL(
struct Params { rows : u32, cols : u32, mode : u32, nu : u32, nw : u32, k : i32, pad0 : u32, pad1 : u32 };
@group(0) @binding(0) var<storage, read> u : array<f32>;
@group(0) @binding(1) var<storage, read> w : array<f32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let index = gid.x;
    if (index >= params.rows * params.cols) { return; }
    let i = index % params.rows;
    let j = index / params.rows;
    var value : f32 = 0.0;
    switch (params.mode) {
        // toeplitz: column u below and on the diagonal, row w above
        case 0u: { if (i >= j) { value = u[i - j]; } else { value = w[j - i]; } }
        // hankel: u down the first column, w along the last row
        case 1u: {
            let s = i + j;
            if (s < params.nu) { value = u[s]; } else if (params.nw > 0u) { value = w[s - params.nu + 1u]; }
        }
        // vander: decreasing powers of u
        case 2u: { value = nlsIpow(u[i], f32(params.cols - 1u - j)); }
        // diagonal matrix of u on diagonal k
        case 3u: {
            let d = i32(j) - i32(i);
            if (d == params.k) { value = u[min(i, j)]; }
        }
        // diagonal k of the matrix u (nu rows), as a column
        case 4u: {
            var r = i;
            var q = i;
            if (params.k >= 0) { q = i + u32(params.k); } else { r = i + u32(-params.k); }
            value = u[r + q * params.nu];
        }
        // companion matrix of the polynomial u
        default: {
            if (i == 0u) { value = -u[j + 1u] / u[0]; } else if (j + 1u == i) { value = 1.0; }
        }
    }
    c[index] = value;
}
)WGSL";
//=============================================================================
ArrayOf
GpuStructuredMatrix(GpuStructuredKind kind, GpuArrayObject* u, GpuArrayObject* w, indexType rows,
    indexType columns, int k, indexType uRows)
{
    GpuArrayObject* output = AllocateGpuArray(Dimensions(rows, columns), "single", false);
    ArrayOf result = ArrayOf::handle(output);
    uint32_t n = (uint32_t)(rows * columns);
    if (n == 0) {
        return result;
    }
    GpuArrayObject* second = (w != nullptr) ? w : u;
    uint32_t params[8] = { (uint32_t)rows, (uint32_t)columns, (uint32_t)kind,
        (uint32_t)(kind == GpuStructuredKind::Diagonal ? uRows : u->elementCount()),
        (uint32_t)(w != nullptr ? w->elementCount() : 0), (uint32_t)k, 0u, 0u };
    std::string source = GpuMathHelpersWgsl() + STRUCTURED_WGSL;
    GpuDispatchKernel(pipeline("gpuArray:structured", source),
        { u->buffer(), second->buffer(), output->buffer() }, params, 8, (n + 255u) / 256u, 1);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
