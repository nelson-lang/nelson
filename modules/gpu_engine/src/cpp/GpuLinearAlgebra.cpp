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
#include <cstring>
#include "GpuLinearAlgebra.hpp"
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
pipeline(const std::string& key, const char* source)
{
    WGPUComputePipeline p = GpuContext::getInstance()->pipelineFor(key, source, "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    return p;
}
//=============================================================================
// element updates per submit, so one submit stays short
static const uint64_t STEP_BUDGET = (uint64_t)1 << 25;
//=============================================================================
// steps sent in submits of bounded work
static void
dispatchInBatches(const std::vector<WGPUBuffer>& buffers, const std::vector<GpuDispatchStep>& steps,
    const std::vector<uint64_t>& work)
{
    std::vector<GpuDispatchStep> batch;
    uint64_t total = 0;
    for (size_t s = 0; s < steps.size(); ++s) {
        batch.push_back(steps[s]);
        total += work[s];
        if (total >= STEP_BUDGET) {
            GpuDispatchSteps(buffers, batch);
            batch.clear();
            total = 0;
        }
    }
    GpuDispatchSteps(buffers, batch);
}
//=============================================================================
// panel columns k0..kEnd-1 in one workgroup: pivot, row swap, multipliers, panel update
static const char* LU_PANEL_WGSL = R"WGSL(
struct Params { m : u32, n : u32, k0 : u32, kEnd : u32, pivoting : u32, pad0 : u32, pad1 : u32, pad2 : u32 };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> piv : array<u32>;
@group(0) @binding(2) var<uniform> params : Params;
var<workgroup> bestValue : array<f32, 256>;
var<workgroup> bestRow : array<u32, 256>;
var<workgroup> pivotRow : array<f32, 64>;
@compute @workgroup_size(256)
fn main(@builtin(local_invocation_id) lid : vec3<u32>) {
    let t = lid.x;
    let m = params.m;
    let k0 = params.k0;
    // largest entry of the first panel column among this thread's rows
    var value : f32 = -1.0;
    var row : u32 = k0;
    for (var i = k0 + t; i < m; i = i + 256u) {
        let v = abs(a[i + k0 * m]);
        if (v > value) { value = v; row = i; }
    }
    for (var k = k0; k < params.kEnd; k = k + 1u) {
        bestValue[t] = value;
        bestRow[t] = row;
        workgroupBarrier();
        for (var s = 128u; s > 0u; s = s >> 1u) {
            if (t < s) {
                let ov = bestValue[t + s];
                let orow = bestRow[t + s];
                if (ov > bestValue[t] || (ov == bestValue[t] && orow < bestRow[t])) {
                    bestValue[t] = ov;
                    bestRow[t] = orow;
                }
            }
            workgroupBarrier();
        }
        let p = select(k, bestRow[0], params.pivoting != 0u);
        // swap rows k and p inside the panel, keep the pivot row in shared memory
        let width = params.kEnd - k0;
        if (t < width) {
            let j = k0 + t;
            let x = a[k + j * m];
            let y = a[p + j * m];
            if (p != k) {
                a[k + j * m] = y;
                a[p + j * m] = x;
            }
            pivotRow[t] = select(x, y, p != k);
        }
        if (t == 0u) { piv[k] = p; }
        storageBarrier();
        workgroupBarrier();
        let pivot = pivotRow[k - k0];
        // multipliers and panel update of this thread's rows, tracking the next pivot
        value = -1.0;
        row = k + 1u;
        for (var i = k + 1u + t; i < m; i = i + 256u) {
            var l = a[i + k * m];
            if (pivot != 0.0) { l = l / pivot; a[i + k * m] = l; }
            for (var j = k + 1u; j < params.kEnd; j = j + 1u) {
                let v = a[i + j * m] - l * pivotRow[j - k0];
                a[i + j * m] = v;
                if (j == k + 1u && abs(v) > value) { value = abs(v); row = i; }
            }
        }
        storageBarrier();
        workgroupBarrier();
    }
}
)WGSL";
//=============================================================================
// one column per thread outside the panel: the panel row swaps, then for columns right of
// the panel the unit lower solve of the block row
static const char* LU_SWAP_WGSL = R"WGSL(
struct Params { m : u32, n : u32, k0 : u32, kEnd : u32 };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> piv : array<u32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    var j = gid.x;
    let width = params.kEnd - params.k0;
    if (j >= params.n - width) { return; }
    if (j >= params.k0) { j = j + width; }
    let m = params.m;
    let base = j * m;
    for (var k = params.k0; k < params.kEnd; k = k + 1u) {
        let p = piv[k];
        if (p != k) {
            let x = a[k + base];
            a[k + base] = a[p + base];
            a[p + base] = x;
        }
    }
    if (j < params.kEnd) { return; }
    for (var i = params.k0 + 1u; i < params.kEnd; i = i + 1u) {
        var s = a[i + base];
        for (var t = params.k0; t < i; t = t + 1u) { s = s - a[i + t * m] * a[t + base]; }
        a[i + base] = s;
    }
}
)WGSL";
//=============================================================================
// C(rows, cols) -= A(rows, k0:kEnd) * B(k0:kEnd, cols), 64x64 tiles with 4x4 values per thread;
// the header declares the bindings, rowStart/rowEnd/colStart/colEnd and loadA/loadB/C
static std::string
tiledUpdateWgsl(const std::string& header)
{
    return header + R"WGSL(
var<workgroup> tileA : array<f32, 1024>;
var<workgroup> tileB : array<f32, 1024>;
@compute @workgroup_size(16, 16)
fn main(@builtin(local_invocation_id) lid : vec3<u32>, @builtin(workgroup_id) wid : vec3<u32>) {
    touchBindings();
    let rowBase = rowStart() + wid.x * 64u;
    let colBase = colStart() + wid.y * 64u;
    let lin = lid.x + lid.y * 16u;
    var acc : array<f32, 16>;
    for (var q = 0u; q < 16u; q = q + 1u) { acc[q] = 0.0; }
    for (var t0 = params.k0; t0 < params.kEnd; t0 = t0 + 16u) {
        // tileA: 64 rows x 16 depth, tileB: 16 depth x 64 columns
        for (var e = lin; e < 1024u; e = e + 256u) {
            let r = e % 64u;
            let d = e / 64u;
            let gi = rowBase + r;
            let gt = t0 + d;
            var va : f32 = 0.0;
            if (gi < rowEnd() && gt < params.kEnd) { va = loadA(gi, gt); }
            tileA[r + d * 64u] = va;
            let dd = e % 16u;
            let c = e / 16u;
            let gt2 = t0 + dd;
            let gj = colBase + c;
            var vb : f32 = 0.0;
            if (gj < colEnd() && gt2 < params.kEnd) { vb = loadB(gt2, gj); }
            tileB[dd + c * 16u] = vb;
        }
        workgroupBarrier();
        for (var d = 0u; d < 16u; d = d + 1u) {
            var ra : array<f32, 4>;
            for (var x = 0u; x < 4u; x = x + 1u) { ra[x] = tileA[lid.x + x * 16u + d * 64u]; }
            for (var y = 0u; y < 4u; y = y + 1u) {
                let rb = tileB[d + (lid.y + y * 16u) * 16u];
                for (var x = 0u; x < 4u; x = x + 1u) { acc[x + y * 4u] = acc[x + y * 4u] + ra[x] * rb; }
            }
        }
        workgroupBarrier();
    }
    for (var y = 0u; y < 4u; y = y + 1u) {
        let j = colBase + lid.y + y * 16u;
        for (var x = 0u; x < 4u; x = x + 1u) {
            let i = rowBase + lid.x + x * 16u;
            if (i < rowEnd() && j < colEnd()) { subtract(i, j, acc[x + y * 4u]); }
        }
    }
}
)WGSL";
}
//=============================================================================
// trailing block of the factorization: A22 -= A21 * A12
static const char* LU_GEMM_HEADER = R"WGSL(
struct Params { m : u32, n : u32, k0 : u32, kEnd : u32 };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> piv : array<u32>;
@group(0) @binding(2) var<uniform> params : Params;
fn touchBindings() { _ = piv[0]; }
fn rowStart() -> u32 { return params.kEnd; }
fn rowEnd() -> u32 { return params.m; }
fn colStart() -> u32 { return params.kEnd; }
fn colEnd() -> u32 { return params.n; }
fn loadA(i : u32, t : u32) -> f32 { return a[i + t * params.m]; }
fn loadB(t : u32, j : u32) -> f32 { return a[t + j * params.m]; }
fn subtract(i : u32, j : u32, v : f32) { a[i + j * params.m] = a[i + j * params.m] - v; }
)WGSL";
//=============================================================================
// block substitution: x(rowStart:rowEnd, :) -= f(rows, k0:kEnd) * x(k0:kEnd, :)
static const char* SOLVE_GEMM_HEADER = R"WGSL(
struct Params { n : u32, nrhs : u32, k0 : u32, kEnd : u32, rowFirst : u32, rowLast : u32, pad0 : u32, pad1 : u32 };
@group(0) @binding(0) var<storage, read> f : array<f32>;
@group(0) @binding(1) var<storage, read> order : array<u32>;
@group(0) @binding(2) var<storage, read> b : array<f32>;
@group(0) @binding(3) var<storage, read_write> x : array<f32>;
@group(0) @binding(4) var<uniform> params : Params;
fn touchBindings() { _ = order[0]; _ = b[0]; }
fn rowStart() -> u32 { return params.rowFirst; }
fn rowEnd() -> u32 { return params.rowLast; }
fn colStart() -> u32 { return 0u; }
fn colEnd() -> u32 { return params.nrhs; }
fn loadA(i : u32, t : u32) -> f32 { return f[i + t * params.n]; }
fn loadB(t : u32, j : u32) -> f32 { return x[t + j * params.n]; }
fn subtract(i : u32, j : u32, v : f32) { x[i + j * params.n] = x[i + j * params.n] - v; }
)WGSL";
//=============================================================================
// substitution helpers on the factor f (n x n); x has nrhs columns
static const char* SOLVE_WGSL = R"WGSL(
struct Params { n : u32, nrhs : u32, k0 : u32, kEnd : u32, mode : u32, pad0 : u32, pad1 : u32, pad2 : u32 };
@group(0) @binding(0) var<storage, read> f : array<f32>;
@group(0) @binding(1) var<storage, read> order : array<u32>;
@group(0) @binding(2) var<storage, read> b : array<f32>;
@group(0) @binding(3) var<storage, read_write> x : array<f32>;
@group(0) @binding(4) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let index = gid.x;
    let n = params.n;
    let k0 = params.k0;
    let kEnd = params.kEnd;
    switch (params.mode) {
        // rows of b in pivot order
        case 0u: {
            if (index >= n * params.nrhs) { return; }
            x[index] = b[order[index % n] + (index / n) * n];
        }
        // permutation matrix: the one of row i is in column order[i]
        case 1u: {
            if (index >= n * params.nrhs) { return; }
            x[index] = select(0.0, 1.0, order[index % n] == index / n);
        }
        // unit lower block k0..kEnd-1, one column per thread
        case 2u: {
            if (index >= params.nrhs) { return; }
            let base = index * n;
            for (var i = k0 + 1u; i < kEnd; i = i + 1u) {
                var s = x[i + base];
                for (var t = k0; t < i; t = t + 1u) { s = s - f[i + t * n] * x[t + base]; }
                x[i + base] = s;
            }
        }
        // upper block k0..kEnd-1 from the bottom, one column per thread
        default: {
            if (index >= params.nrhs) { return; }
            let base = index * n;
            for (var r = kEnd; r > k0; r = r - 1u) {
                let i = r - 1u;
                var s = x[i + base];
                for (var t = i + 1u; t < kEnd; t = t + 1u) { s = s - f[i + t * n] * x[t + base]; }
                x[i + base] = s / f[i + i * n];
            }
        }
    }
}
)WGSL";
//=============================================================================
// parts of the factor f (m x n): unit lower, permuted lower, upper, diagonal, Cholesky upper and
// lower (rows of U scaled by the root of their pivot), symmetric matrix of the upper triangle
static const char* EXTRACT_WGSL = R"WGSL(
struct Params { m : u32, rows : u32, cols : u32, mode : u32 };
@group(0) @binding(0) var<storage, read> f : array<f32>;
@group(0) @binding(1) var<storage, read> order : array<u32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let index = gid.x;
    if (index >= params.rows * params.cols) { return; }
    var i = index % params.rows;
    let j = index / params.rows;
    let m = params.m;
    var value : f32 = 0.0;
    if (params.mode <= 1u) {
        if (params.mode == 1u) { i = order[i]; }
        if (i > j) { value = f[i + j * m]; } else if (i == j) { value = 1.0; }
    } else if (params.mode == 2u) {
        if (i <= j) { value = f[i + j * m]; }
    } else if (params.mode == 3u) {
        value = f[i + i * m];
    } else if (params.mode == 4u) {
        if (i <= j) { value = f[i + j * m] / sqrt(f[i + i * m]); }
    } else if (params.mode == 5u) {
        if (i >= j) { value = f[j + i * m] / sqrt(f[j + j * m]); }
    } else {
        value = f[min(i, j) + max(i, j) * m];
    }
    c[index] = value;
}
)WGSL";
//=============================================================================
// workgroup sum of one value per thread (256 threads), result in partial[0]
static const char* QR_REDUCE_WGSL = R"WGSL(
var<workgroup> partial : array<f32, 256>;
fn workgroupSum(t : u32, value : f32) -> f32 {
    partial[t] = value;
    workgroupBarrier();
    for (var s = 128u; s > 0u; s = s >> 1u) {
        if (t < s) { partial[t] = partial[t] + partial[t + s]; }
        workgroupBarrier();
    }
    let total = partial[0];
    workgroupBarrier();
    return total;
}
)WGSL";
//=============================================================================
// Householder reflector of column k (one workgroup): beta on the diagonal, v below with v(k) = 1
static const char* QR_HOUSE_WGSL = R"WGSL(
struct Params { m : u32, n : u32, k : u32, pad : u32 };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> tau : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
var<workgroup> largest : array<f32, 256>;
@compute @workgroup_size(256)
fn main(@builtin(local_invocation_id) lid : vec3<u32>) {
    let t = lid.x;
    let m = params.m;
    let k = params.k;
    let col = k * m;
    // read before the barriers: thread 0 overwrites it with beta at the end
    let alpha = a[col + k];
    // scale by the largest entry below the diagonal so the squares stay finite
    var big : f32 = 0.0;
    for (var i = k + 1u + t; i < m; i = i + 256u) { big = max(big, abs(a[col + i])); }
    largest[t] = big;
    workgroupBarrier();
    for (var s = 128u; s > 0u; s = s >> 1u) {
        if (t < s) { largest[t] = max(largest[t], largest[t + s]); }
        workgroupBarrier();
    }
    let scale = largest[0];
    var sq : f32 = 0.0;
    if (scale > 0.0) {
        for (var i = k + 1u + t; i < m; i = i + 256u) {
            let x = a[col + i] / scale;
            sq = sq + x * x;
        }
    }
    let total = workgroupSum(t, sq);
    if (scale == 0.0) {
        if (t == 0u) { tau[k] = 0.0; }
        return;
    }
    let xnorm = scale * sqrt(total);
    let size = max(abs(alpha), xnorm);
    let norm = size * sqrt((alpha / size) * (alpha / size) + (xnorm / size) * (xnorm / size));
    let beta = select(norm, -norm, alpha >= 0.0);
    let factor = 1.0 / (alpha - beta);
    for (var i = k + 1u + t; i < m; i = i + 256u) { a[col + i] = a[col + i] * factor; }
    if (t == 0u) {
        a[col + k] = beta;
        tau[k] = (beta - alpha) / beta;
    }
}
)WGSL";
//=============================================================================
// reflector k applied to one column per workgroup of the target: x -= tau * v * (v' * x)
static std::string
qrApplyWgsl(bool separateTarget)
{
    std::string bindings = separateTarget ? R"WGSL(
struct Params { m : u32, cols : u32, k : u32, first : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> tau : array<f32>;
@group(0) @binding(2) var<storage, read_write> x : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
fn vk(i : u32) -> f32 { return a[i + params.k * params.m]; }
fn getX(i : u32) -> f32 { return x[i]; }
fn setX(i : u32, value : f32) { x[i] = value; }
)WGSL"
                                          : R"WGSL(
struct Params { m : u32, cols : u32, k : u32, first : u32 };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> tau : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
fn vk(i : u32) -> f32 { return a[i + params.k * params.m]; }
fn getX(i : u32) -> f32 { return a[i]; }
fn setX(i : u32, value : f32) { a[i] = value; }
)WGSL";
    return bindings + QR_REDUCE_WGSL + R"WGSL(
@compute @workgroup_size(256)
fn main(@builtin(local_invocation_id) lid : vec3<u32>, @builtin(workgroup_id) wid : vec3<u32>) {
    let t = lid.x;
    let m = params.m;
    let k = params.k;
    let j = params.first + wid.x + wid.y * 65535u;
    // the whole workgroup takes the same path; the sum below needs every thread
    let working = j < params.cols && tau[k] != 0.0;
    let base = j * m;
    var dotPart : f32 = 0.0;
    // read before the barriers: thread 0 overwrites it at the end
    var head : f32 = 0.0;
    if (working) {
        head = getX(k + base);
        for (var i = k + 1u + t; i < m; i = i + 256u) { dotPart = dotPart + vk(i) * getX(i + base); }
    }
    let total = workgroupSum(t, dotPart);
    if (!working) { return; }
    let w = tau[k] * (head + total);
    for (var i = k + 1u + t; i < m; i = i + 256u) { setX(i + base, getX(i + base) - vk(i) * w); }
    if (t == 0u) { setX(k + base, getX(k + base) - w); }
}
)WGSL";
}
//=============================================================================
namespace {
    //=============================================================================
    struct LuFactor
    {
        ArrayOf owner;
        GpuArrayObject* factor = nullptr;
        // perm[i]: row of the input that ends at row i
        std::vector<uint32_t> perm;
        uint32_t swaps = 0;
    };
    //=============================================================================
    ArrayOf
    uploadOrder(const std::vector<uint32_t>& values)
    {
        uint32_t* data = (uint32_t*)ArrayOf::allocateArrayOf(NLS_UINT32, values.size());
        std::copy(values.begin(), values.end(), data);
        return CreateGpuArray(ArrayOf(NLS_UINT32, Dimensions(values.size(), 1), data));
    }
    //=============================================================================
    // pivoting false: no row exchange; fromUpper: start from the symmetric matrix of the upper
    // triangle
    void
    luFactor(GpuArrayObject* a, LuFactor& lu, bool pivoting = true, bool fromUpper = false)
    {
        uint32_t m = (uint32_t)a->dimensions().rows();
        uint32_t n = (uint32_t)a->dimensions().columns();
        uint32_t r = std::min(m, n);
        GpuArrayObject* piv = AllocateGpuArray(Dimensions(1, r), "uint32", false);
        ArrayOf pivOwner = ArrayOf::handle(piv);
        if (fromUpper) {
            lu.factor = AllocateGpuArray(a->dimensions(), "single", false);
            lu.owner = ArrayOf::handle(lu.factor);
            uint32_t params[4] = { m, m, n, 6u };
            GpuDispatchKernel(pipeline("gpuArray:luextract", EXTRACT_WGSL),
                { a->buffer(), piv->buffer(), lu.factor->buffer() }, params, 4,
                (uint32_t)(((uint64_t)m * n + 255u) / 256u), 1);
        } else {
            lu.factor = CloneGpuArray(a);
            lu.owner = ArrayOf::handle(lu.factor);
        }
        WGPUComputePipeline panelKernel = pipeline("gpuArray:lupanel", LU_PANEL_WGSL);
        WGPUComputePipeline swap = pipeline("gpuArray:luswap", LU_SWAP_WGSL);
        WGPUComputePipeline gemm
            = pipeline("gpuArray:lugemm", tiledUpdateWgsl(LU_GEMM_HEADER).c_str());
        std::vector<GpuDispatchStep> steps;
        std::vector<uint64_t> work;
        // blocked right-looking: factor a panel, swap and solve its block row, update the rest
        const uint32_t panel = 16;
        for (uint32_t k0 = 0; k0 < r; k0 += panel) {
            uint32_t kEnd = std::min(k0 + panel, r);
            steps.push_back(
                { panelKernel, { m, n, k0, kEnd, pivoting ? 1u : 0u, 0u, 0u, 0u }, 1u, 1u, 1u });
            work.push_back((uint64_t)(m - k0) * panel * panel / 2);
            if (n > kEnd - k0) {
                uint32_t others = n - (kEnd - k0);
                steps.push_back({ swap, { m, n, k0, kEnd }, (others + 255u) / 256u, 1u, 1u });
                work.push_back((uint64_t)others * panel * panel / 2);
            }
            if (kEnd < n && kEnd < m) {
                uint32_t rows = m - kEnd;
                uint32_t cols = n - kEnd;
                steps.push_back(
                    { gemm, { m, n, k0, kEnd }, (rows + 63u) / 64u, (cols + 63u) / 64u, 1u });
                work.push_back((uint64_t)rows * cols * (kEnd - k0) / 16);
            }
        }
        dispatchInBatches({ lu.factor->buffer(), piv->buffer() }, steps, work);
        ArrayOf host = GatherGpuArray(piv);
        const uint32_t* p = (const uint32_t*)host.dataPointer();
        lu.perm.resize(m);
        for (uint32_t i = 0; i < m; ++i) {
            lu.perm[i] = i;
        }
        lu.swaps = 0;
        for (uint32_t k = 0; k < r; ++k) {
            if (p[k] != k) {
                std::swap(lu.perm[k], lu.perm[p[k]]);
                lu.swaps++;
            }
        }
    }
    //=============================================================================
    ArrayOf
    extract(const LuFactor& lu, GpuArrayObject* order, uint32_t rows, uint32_t cols, uint32_t mode)
    {
        GpuArrayObject* out = AllocateGpuArray(Dimensions(rows, cols), "single", false);
        ArrayOf result = ArrayOf::handle(out);
        uint32_t total = rows * cols;
        if (total > 0) {
            uint32_t params[4] = { (uint32_t)lu.factor->dimensions().rows(), rows, cols, mode };
            GpuDispatchKernel(pipeline("gpuArray:luextract", EXTRACT_WGSL),
                { lu.factor->buffer(), order->buffer(), out->buffer() }, params, 4,
                (total + 255u) / 256u, 1);
        }
        return result;
    }
    //=============================================================================
    // true when the factor has an exact zero on its diagonal
    bool
    singular(const LuFactor& lu, GpuArrayObject* order, std::vector<double>* diagonal = nullptr)
    {
        uint32_t r
            = (uint32_t)std::min(lu.factor->dimensions().rows(), lu.factor->dimensions().columns());
        ArrayOf d = extract(lu, order, r, 1, 3);
        ArrayOf host = GatherGpuArray(AsGpuArrayObject(d));
        const single* p = (const single*)host.dataPointer();
        bool zero = false;
        for (uint32_t k = 0; k < r; ++k) {
            zero = zero || p[k] == 0.0f;
            if (diagonal) {
                diagonal->push_back((double)p[k]);
            }
        }
        return zero;
    }
    //=============================================================================
    // x = f \ (rows of b in order), or f \ P when b is null
    ArrayOf
    solveFactored(const LuFactor& lu, GpuArrayObject* order, GpuArrayObject* b, uint32_t nrhs)
    {
        uint32_t n = (uint32_t)lu.factor->dimensions().rows();
        GpuArrayObject* x = AllocateGpuArray(Dimensions(n, nrhs), "single", false);
        ArrayOf result = ArrayOf::handle(x);
        WGPUComputePipeline solve = pipeline("gpuArray:lusolve", SOLVE_WGSL);
        WGPUComputePipeline update
            = pipeline("gpuArray:lusolveupdate", tiledUpdateWgsl(SOLVE_GEMM_HEADER).c_str());
        const uint32_t block = 32;
        uint32_t columnGroups = (nrhs + 255u) / 256u;
        uint32_t tileColumns = (nrhs + 63u) / 64u;
        std::vector<GpuDispatchStep> steps;
        std::vector<uint64_t> work;
        steps.push_back({ solve, { n, nrhs, 0u, 0u, b ? 0u : 1u, 0u, 0u, 0u },
            (uint32_t)(((uint64_t)n * nrhs + 255u) / 256u), 1u, 1u });
        work.push_back((uint64_t)n * nrhs);
        // forward: solve a block, then remove it from the rows below
        for (uint32_t k0 = 0; k0 < n; k0 += block) {
            uint32_t kEnd = std::min(k0 + block, n);
            steps.push_back({ solve, { n, nrhs, k0, kEnd, 2u, 0u, 0u, 0u }, columnGroups, 1u, 1u });
            work.push_back((uint64_t)nrhs * block * block / 2);
            if (kEnd < n) {
                steps.push_back({ update, { n, nrhs, k0, kEnd, kEnd, n, 0u, 0u },
                    (n - kEnd + 63u) / 64u, tileColumns, 1u });
                work.push_back((uint64_t)(n - kEnd) * nrhs * block / 16);
            }
        }
        // backward: from the last block up, then remove it from the rows above
        uint32_t lastStart = ((n - 1) / block) * block;
        for (int64_t s = lastStart; s >= 0; s -= block) {
            uint32_t k0 = (uint32_t)s;
            uint32_t kEnd = std::min(k0 + block, n);
            steps.push_back({ solve, { n, nrhs, k0, kEnd, 3u, 0u, 0u, 0u }, columnGroups, 1u, 1u });
            work.push_back((uint64_t)nrhs * block * block / 2);
            if (k0 > 0) {
                steps.push_back({ update, { n, nrhs, k0, kEnd, 0u, k0, 0u, 0u }, (k0 + 63u) / 64u,
                    tileColumns, 1u });
                work.push_back((uint64_t)k0 * nrhs * block / 16);
            }
        }
        WGPUBuffer source = b ? b->buffer() : lu.factor->buffer();
        dispatchInBatches(
            { lu.factor->buffer(), order->buffer(), source, x->buffer() }, steps, work);
        return result;
    }
    //=============================================================================
    // sizes the kernels index with 32-bit values and 2-D grids
    bool
    supported(GpuArrayObject* a)
    {
        Dimensions d = a->dimensions();
        return GpuIsRealSingle(a) && d.length() <= 2 && a->elementCount() > 0 && d.rows() <= 1000000
            && d.columns() <= 1000000;
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
GpuIsRealSingle(GpuArrayObject* a)
{
    return a != nullptr && !a->isComplex() && a->underlyingType() == "single";
}
//=============================================================================
bool
GpuLu(GpuArrayObject* a, int nLhs, bool vectorForm, ArrayOfVector& outputs)
{
    if (!supported(a)) {
        return false;
    }
    uint32_t m = (uint32_t)a->dimensions().rows();
    uint32_t n = (uint32_t)a->dimensions().columns();
    uint32_t r = std::min(m, n);
    LuFactor lu;
    luFactor(a, lu);
    outputs.clear();
    if (nLhs <= 1) {
        outputs << lu.owner;
        return true;
    }
    if (nLhs == 2) {
        std::vector<uint32_t> inverse(m);
        for (uint32_t i = 0; i < m; ++i) {
            inverse[lu.perm[i]] = i;
        }
        ArrayOf order = uploadOrder(inverse);
        outputs << extract(lu, AsGpuArrayObject(order), m, r, 1);
        outputs << extract(lu, AsGpuArrayObject(order), r, n, 2);
        return true;
    }
    ArrayOf order = uploadOrder(lu.perm);
    outputs << extract(lu, AsGpuArrayObject(order), m, r, 0);
    outputs << extract(lu, AsGpuArrayObject(order), r, n, 2);
    if (vectorForm) {
        single* p = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, m);
        for (uint32_t i = 0; i < m; ++i) {
            p[i] = (single)(lu.perm[i] + 1);
        }
        outputs << CreateGpuArray(ArrayOf(NLS_SINGLE, Dimensions(1, m), p));
        return true;
    }
    // the permutation matrix comes from the solve kernel run on its first step only
    GpuArrayObject* pm = AllocateGpuArray(Dimensions(m, m), "single", false);
    ArrayOf pmOwner = ArrayOf::handle(pm);
    uint32_t params[8] = { m, m, 0u, 0u, 1u, 0u, 0u, 0u };
    GpuDispatchKernel(pipeline("gpuArray:lusolve", SOLVE_WGSL),
        { lu.factor->buffer(), AsGpuArrayObject(order)->buffer(), lu.factor->buffer(),
            pm->buffer() },
        params, 8, (uint32_t)(((uint64_t)m * m + 255u) / 256u), 1);
    outputs << pmOwner;
    return true;
}
//=============================================================================
bool
GpuDet(GpuArrayObject* a, ArrayOf& result)
{
    if (!supported(a) || a->dimensions().rows() != a->dimensions().columns()) {
        return false;
    }
    LuFactor lu;
    luFactor(a, lu);
    ArrayOf order = uploadOrder(lu.perm);
    std::vector<double> diagonal;
    singular(lu, AsGpuArrayObject(order), &diagonal);
    double value = (lu.swaps % 2) ? -1.0 : 1.0;
    for (double d : diagonal) {
        value *= d;
    }
    result = CreateGpuArray(ArrayOf::singleScalar((single)value));
    return true;
}
//=============================================================================
bool
GpuSolveSquare(GpuArrayObject* a, GpuArrayObject* b, ArrayOf& result)
{
    if (!supported(a) || !supported(b) || a->dimensions().rows() != a->dimensions().columns()
        || b->dimensions().rows() != a->dimensions().rows()) {
        return false;
    }
    LuFactor lu;
    luFactor(a, lu);
    ArrayOf order = uploadOrder(lu.perm);
    // singular systems keep the host answer and warning
    if (singular(lu, AsGpuArrayObject(order))) {
        return false;
    }
    result = solveFactored(lu, AsGpuArrayObject(order), b, (uint32_t)b->dimensions().columns());
    return true;
}
//=============================================================================
bool
GpuInv(GpuArrayObject* a, ArrayOf& result)
{
    if (!supported(a) || a->dimensions().rows() != a->dimensions().columns()) {
        return false;
    }
    LuFactor lu;
    luFactor(a, lu);
    ArrayOf order = uploadOrder(lu.perm);
    if (singular(lu, AsGpuArrayObject(order))) {
        return false;
    }
    result
        = solveFactored(lu, AsGpuArrayObject(order), nullptr, (uint32_t)a->dimensions().columns());
    return true;
}
//=============================================================================
bool
GpuChol(GpuArrayObject* a, bool lower, ArrayOf& result)
{
    if (!supported(a) || a->dimensions().rows() != a->dimensions().columns()) {
        return false;
    }
    uint32_t n = (uint32_t)a->dimensions().rows();
    LuFactor lu;
    luFactor(a, lu, false, true);
    ArrayOf order = uploadOrder(lu.perm);
    std::vector<double> diagonal;
    singular(lu, AsGpuArrayObject(order), &diagonal);
    // a matrix that is not positive definite keeps the host error and partial factor
    for (double d : diagonal) {
        if (!(d > 0.0)) {
            return false;
        }
    }
    result = extract(lu, AsGpuArrayObject(order), n, n, lower ? 5u : 4u);
    return true;
}
//=============================================================================
// workgroups over the columns first..cols-1 of a reflector application, folded into 2-D
static void
qrApplyGrid(uint32_t count, uint32_t& x, uint32_t& y)
{
    x = std::min<uint32_t>(count, 65535u);
    y = (count + 65534u) / 65535u;
}
//=============================================================================
bool
GpuQr(GpuArrayObject* a, int nLhs, bool economy, ArrayOfVector& outputs)
{
    if (!supported(a) || nLhs > 2) {
        return false;
    }
    uint32_t m = (uint32_t)a->dimensions().rows();
    uint32_t n = (uint32_t)a->dimensions().columns();
    uint32_t r = std::min(m, n);
    LuFactor qr;
    qr.factor = CloneGpuArray(a);
    qr.owner = ArrayOf::handle(qr.factor);
    GpuArrayObject* tau
        = AllocateGpuArray(Dimensions(1, std::max<uint32_t>(r, 1)), "single", false);
    ArrayOf tauOwner = ArrayOf::handle(tau);
    WGPUComputePipeline house
        = pipeline("gpuArray:qrhouse", (std::string(QR_REDUCE_WGSL) + QR_HOUSE_WGSL).c_str());
    WGPUComputePipeline apply = pipeline("gpuArray:qrapply", qrApplyWgsl(false).c_str());
    std::vector<GpuDispatchStep> steps;
    std::vector<uint64_t> work;
    // reflectors for the columns that have rows below the diagonal
    uint32_t reflectors = std::min(m > 0 ? m - 1 : 0, n);
    for (uint32_t k = 0; k < reflectors; ++k) {
        steps.push_back({ house, { m, n, k, 0u }, 1u, 1u, 1u });
        work.push_back(m - k);
        if (k + 1 < n) {
            uint32_t gx, gy;
            qrApplyGrid(n - k - 1, gx, gy);
            steps.push_back({ apply, { m, n, k, k + 1 }, gx, gy, 1u });
            work.push_back((uint64_t)(m - k) * (n - k - 1));
        }
    }
    dispatchInBatches({ qr.factor->buffer(), tau->buffer() }, steps, work);
    uint32_t rRows = economy ? r : m;
    std::vector<uint32_t> identity(m);
    for (uint32_t i = 0; i < m; ++i) {
        identity[i] = i;
    }
    ArrayOf order = uploadOrder(identity);
    if (nLhs <= 1) {
        outputs << extract(qr, AsGpuArrayObject(order), rRows, n, 2);
        return true;
    }
    // Q: the reflectors applied in reverse order to the first columns of the identity
    uint32_t qCols = economy ? r : m;
    GpuArrayObject* q = AllocateGpuArray(Dimensions(m, qCols), "single", false);
    ArrayOf qOwner = ArrayOf::handle(q);
    uint32_t eye[8] = { m, qCols, 0u, 0u, 1u, 0u, 0u, 0u };
    GpuDispatchKernel(pipeline("gpuArray:lusolve", SOLVE_WGSL),
        { qr.factor->buffer(), AsGpuArrayObject(order)->buffer(), qr.factor->buffer(),
            q->buffer() },
        eye, 8, (uint32_t)(((uint64_t)m * qCols + 255u) / 256u), 1);
    WGPUComputePipeline applyQ = pipeline("gpuArray:qrapplyq", qrApplyWgsl(true).c_str());
    std::vector<GpuDispatchStep> qSteps;
    std::vector<uint64_t> qWork;
    for (uint32_t s = reflectors; s > 0; --s) {
        uint32_t k = s - 1;
        uint32_t gx, gy;
        qrApplyGrid(qCols, gx, gy);
        qSteps.push_back({ applyQ, { m, qCols, k, 0u }, gx, gy, 1u });
        qWork.push_back((uint64_t)(m - k) * qCols);
    }
    dispatchInBatches({ qr.factor->buffer(), tau->buffer(), q->buffer() }, qSteps, qWork);
    outputs << qOwner;
    outputs << extract(qr, AsGpuArrayObject(order), rRows, n, 2);
    return true;
}
//=============================================================================
// block one-sided Jacobi: workgroup k takes the block pair k of the round (8 columns per block)
// and rotates all pairs of its 16 columns in 15 inner rounds, 32 threads per column pair
static const char* JACOBI_BLOCK_WGSL = R"WGSL(
struct Params { m : u32, n : u32, slots : u32, round : u32, sweep : u32, pad0 : u32, pad1 : u32, pad2 : u32 };
@group(0) @binding(0) var<storage, read_write> w : array<f32>;
@group(0) @binding(1) var<storage, read_write> v : array<f32>;
@group(0) @binding(2) var<storage, read_write> flag : array<atomic<u32>>;
@group(0) @binding(3) var<uniform> params : Params;
const BLOCK : u32 = 4u;
const LANES : u32 = 64u;
var<workgroup> sa : array<f32, 256>;
var<workgroup> sb : array<f32, 256>;
var<workgroup> sg : array<f32, 256>;
// position in a round-robin tournament (slot 0 stays, the others turn)
fn player(position : u32, round : u32, slots : u32) -> u32 {
    if (position == 0u) { return 0u; }
    return ((position - 1u + round) % (slots - 1u)) + 1u;
}
@compute @workgroup_size(256)
fn main(@builtin(local_invocation_id) lid : vec3<u32>, @builtin(workgroup_id) wid : vec3<u32>) {
    let t = lid.x;
    let m = params.m;
    let n = params.n;
    let blockP = player(wid.x, params.round, params.slots);
    let blockQ = player(params.slots - 1u - wid.x, params.round, params.slots);
    let pair = t / LANES;
    let lane = t % LANES;
    for (var inner = 0u; inner < 2u * BLOCK - 1u; inner = inner + 1u) {
        let first = player(pair, inner, 2u * BLOCK);
        let second = player(2u * BLOCK - 1u - pair, inner, 2u * BLOCK);
        let localP = min(first, second);
        let localQ = max(first, second);
        let p = select(blockQ, blockP, localP < BLOCK) * BLOCK + (localP % BLOCK);
        let q = select(blockQ, blockP, localQ < BLOCK) * BLOCK + (localQ % BLOCK);
        let real = p < n && q < n && p != q;
        var a : f32 = 0.0;
        var b : f32 = 0.0;
        var g : f32 = 0.0;
        if (real) {
            for (var i = lane; i < m; i = i + LANES) {
                let x = w[i + p * m];
                let y = w[i + q * m];
                a = a + x * x;
                b = b + y * y;
                g = g + x * y;
            }
        }
        sa[t] = a;
        sb[t] = b;
        sg[t] = g;
        workgroupBarrier();
        for (var s = LANES / 2u; s > 0u; s = s >> 1u) {
            if (lane < s) {
                sa[t] = sa[t] + sa[t + s];
                sb[t] = sb[t] + sb[t + s];
                sg[t] = sg[t] + sg[t + s];
            }
            workgroupBarrier();
        }
        let base = pair * LANES;
        let alpha = sa[base];
        let beta = sb[base];
        let gamma = sg[base];
        workgroupBarrier();
        var rotate = real && gamma != 0.0;
        var overlap : f32 = 0.0;
        if (rotate) {
            overlap = abs(gamma) / sqrt(alpha * beta);
            rotate = overlap > 1e-7;
        }
        if (lane == 0u && real && gamma != 0.0) { atomicMax(&flag[params.sweep], bitcast<u32>(overlap)); }
        if (rotate) {
            let zeta = (beta - alpha) / (2.0 * gamma);
            let tangent = select(-1.0, 1.0, zeta >= 0.0) / (abs(zeta) + sqrt(1.0 + zeta * zeta));
            let c = 1.0 / sqrt(1.0 + tangent * tangent);
            let sn = c * tangent;
            for (var i = lane; i < m; i = i + LANES) {
                let x = w[i + p * m];
                let y = w[i + q * m];
                w[i + p * m] = c * x - sn * y;
                w[i + q * m] = sn * x + c * y;
            }
            for (var i = lane; i < n; i = i + LANES) {
                let x = v[i + p * n];
                let y = v[i + q * n];
                v[i + p * n] = c * x - sn * y;
                v[i + q * n] = sn * x + c * y;
            }
        }
        storageBarrier();
        workgroupBarrier();
    }
}
)WGSL";
//=============================================================================
bool
GpuJacobiSvd(GpuArrayObject* a, ArrayOf& w, ArrayOf& v)
{
    if (!GpuIsRealSingle(a) || a->dimensions().length() > 2 || a->elementCount() == 0) {
        return false;
    }
    uint32_t m = (uint32_t)a->dimensions().rows();
    uint32_t n = (uint32_t)a->dimensions().columns();
    if (m < n || n > 65535u * 2u) {
        return false;
    }
    GpuArrayObject* wObject = CloneGpuArray(a);
    w = ArrayOf::handle(wObject);
    std::vector<uint32_t> identity(n);
    for (uint32_t i = 0; i < n; ++i) {
        identity[i] = i;
    }
    ArrayOf order = uploadOrder(identity);
    GpuArrayObject* vObject = AllocateGpuArray(Dimensions(n, n), "single", false);
    v = ArrayOf::handle(vObject);
    uint32_t eye[8] = { n, n, 0u, 0u, 1u, 0u, 0u, 0u };
    GpuDispatchKernel(pipeline("gpuArray:lusolve", SOLVE_WGSL),
        { wObject->buffer(), AsGpuArrayObject(order)->buffer(), wObject->buffer(),
            vObject->buffer() },
        eye, 8, (uint32_t)(((uint64_t)n * n + 255u) / 256u), 1);
    if (n == 1) {
        return true;
    }
    const uint32_t maxSweeps = 40;
    ArrayOf flagOwner = GpuFilled(Dimensions(1, maxSweeps), 0.0f, "uint32");
    GpuArrayObject* flag = AsGpuArrayObject(flagOwner);
    WGPUComputePipeline round = pipeline("gpuArray:jacobiblock", JACOBI_BLOCK_WGSL);
    const uint32_t block = 4u;
    uint32_t blocks = (n + block - 1u) / block;
    uint32_t slots = blocks + (blocks % 2u);
    uint32_t pairs = slots / 2u;
    for (uint32_t sweep = 0; sweep < maxSweeps; ++sweep) {
        std::vector<GpuDispatchStep> steps;
        std::vector<uint64_t> work;
        for (uint32_t r = 0; r + 1 < slots; ++r) {
            steps.push_back({ round, { m, n, slots, r, sweep, 0u, 0u, 0u }, pairs, 1u, 1u });
            work.push_back((uint64_t)pairs * (2u * block - 1u) * block * (m + n));
        }
        dispatchInBatches({ wObject->buffer(), vObject->buffer(), flag->buffer() }, steps, work);
        ArrayOf host = GatherGpuArray(flag);
        uint32_t bits = ((const uint32_t*)host.dataPointer())[sweep];
        float largest;
        std::memcpy(&largest, &bits, sizeof(largest));
        if (!(largest > 1e-7f)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
