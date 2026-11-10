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
#include "GpuFft.hpp"
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
fftPipeline(const std::string& key, const char* body)
{
    std::string source = GpuMathHelpersWgsl() + body;
    WGPUComputePipeline p = GpuContext::getInstance()->pipelineFor(key, source.c_str(), "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    return p;
}
//=============================================================================
static uint32_t
groups(uint64_t threads)
{
    return (uint32_t)((threads + 255u) / 256u);
}
//=============================================================================
// (k * k) mod m without overflow, then the chirp exp(i * sign * pi * k^2 / n)
static const char* CHIRP_WGSL = R"WGSL(
fn mulmod(a : u32, b : u32, m : u32) -> u32 {
    var r = 0u;
    var x = a % m;
    var y = b;
    loop {
        if (y == 0u) { break; }
        if ((y & 1u) != 0u) { r = (r + x) % m; }
        x = (x + x) % m;
        y = y >> 1u;
    }
    return r;
}
fn chirp(k : u32, n : u32, sign : f32) -> vec2<f32> {
    let q = mulmod(k, k, 2u * n);
    let sc = nlsSinCos(sign * NLS_PI * f32(q) / f32(n));
    return vec2<f32>(sc.y, sc.x);
}
fn cmul(a : vec2<f32>, b : vec2<f32>) -> vec2<f32> {
    return vec2<f32>(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}
)WGSL";
//=============================================================================
// lanes of the input copied into lane-major complex rows of length L (zero padded); with
// chirp != 0 every value k < n is multiplied by the chirp of sign `sign`
static const char* LOAD_WGSL = R"WGSL(
struct Params { total : u32, L : u32, inLen : u32, before : u32, n : u32, complex : u32,
    chirp : u32, sign : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> w : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.total) { return; }
    let lane = i / params.L;
    let k = i % params.L;
    var v = vec2<f32>(0.0, 0.0);
    if (k < min(params.inLen, params.n)) {
        let start = lane % params.before + (lane / params.before) * params.before * params.inLen;
        let e = start + k * params.before;
        if (params.complex == 1u) { v = vec2<f32>(a[2u * e], a[2u * e + 1u]); } else { v = vec2<f32>(a[e], 0.0); }
        if (params.chirp == 1u) { v = cmul(v, chirp(k, params.n, bitcast<f32>(params.sign))); }
    }
    w[2u * i] = v.x;
    w[2u * i + 1u] = v.y;
}
)WGSL";
//=============================================================================
// one radix-2 Stockham pass; parity picks the read and write buffers
static const char* STOCKHAM_WGSL = R"WGSL(
struct Params { total : u32, L : u32, ns : u32, parity : u32, sign : u32 };
@group(0) @binding(0) var<storage, read_write> x : array<f32>;
@group(0) @binding(1) var<storage, read_write> y : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
fn readValue(i : u32) -> vec2<f32> {
    if (params.parity == 0u) { return vec2<f32>(x[2u * i], x[2u * i + 1u]); }
    return vec2<f32>(y[2u * i], y[2u * i + 1u]);
}
fn writeValue(i : u32, v : vec2<f32>) {
    if (params.parity == 0u) { y[2u * i] = v.x; y[2u * i + 1u] = v.y; }
    else { x[2u * i] = v.x; x[2u * i + 1u] = v.y; }
}
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let t = gid.x;
    if (t >= params.total) { return; }
    let halfLen = params.L / 2u;
    let lane = t / halfLen;
    let j = t % halfLen;
    let base = lane * params.L;
    let k = j % params.ns;
    let sc = nlsSinCos(bitcast<f32>(params.sign) * NLS_PI * f32(k) / f32(params.ns));
    let v0 = readValue(base + j);
    let v1 = cmul(readValue(base + j + halfLen), vec2<f32>(sc.y, sc.x));
    let d = (j / params.ns) * 2u * params.ns + k;
    writeValue(base + d, v0 + v1);
    writeValue(base + d + params.ns, v0 - v1);
}
)WGSL";
//=============================================================================
// Bluestein filter conj(chirp), wrapped on a row of length L
static const char* FILTER_WGSL = R"WGSL(
struct Params { L : u32, n : u32, sign : u32 };
@group(0) @binding(0) var<storage, read_write> w : array<f32>;
@group(0) @binding(1) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let j = gid.x;
    if (j >= params.L) { return; }
    var v = vec2<f32>(0.0, 0.0);
    let s = -bitcast<f32>(params.sign);
    if (j < params.n) { v = chirp(j, params.n, s); }
    else if (j > params.L - params.n) { v = chirp(params.L - j, params.n, s); }
    w[2u * j] = v.x;
    w[2u * j + 1u] = v.y;
}
)WGSL";
//=============================================================================
// rows of w multiplied by the row f
static const char* MULTIPLY_WGSL = R"WGSL(
struct Params { total : u32, L : u32 };
@group(0) @binding(0) var<storage, read> f : array<f32>;
@group(0) @binding(1) var<storage, read_write> w : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.total) { return; }
    let j = i % params.L;
    let v = cmul(vec2<f32>(w[2u * i], w[2u * i + 1u]), vec2<f32>(f[2u * j], f[2u * j + 1u]));
    w[2u * i] = v.x;
    w[2u * i + 1u] = v.y;
}
)WGSL";
//=============================================================================
// rows (length L) written back along the transformed dimension (length n) with a scale
// and, for Bluestein, the final chirp
static const char* STORE_WGSL = R"WGSL(
struct Params { total : u32, L : u32, n : u32, before : u32, scale : u32, chirp : u32,
    sign : u32, pad : u32 };
@group(0) @binding(0) var<storage, read> w : array<f32>;
@group(0) @binding(1) var<storage, read_write> c : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.total) { return; }
    let lane = i / params.n;
    let k = i % params.n;
    var v = vec2<f32>(w[2u * (lane * params.L + k)], w[2u * (lane * params.L + k) + 1u]);
    if (params.chirp == 1u) { v = cmul(v, chirp(k, params.n, bitcast<f32>(params.sign))); }
    v = v * bitcast<f32>(params.scale);
    let start = lane % params.before + (lane / params.before) * params.before * params.n;
    let e = start + k * params.before;
    c[2u * e] = v.x;
    c[2u * e + 1u] = v.y;
}
)WGSL";
//=============================================================================
static uint32_t
floatBits(float v)
{
    uint32_t b;
    std::memcpy(&b, &v, sizeof(b));
    return b;
}
//=============================================================================
// in-place power-of-two transform of `lanes` rows of length L held in x (y is scratch);
// returns the buffer holding the result
static WGPUBuffer
stockham(WGPUBuffer x, WGPUBuffer y, uint32_t lanes, uint32_t L, float sign)
{
    if (L < 2) {
        return x;
    }
    std::vector<std::vector<uint32_t>> passes;
    uint32_t parity = 0;
    for (uint32_t ns = 1; ns < L; ns <<= 1) {
        passes.push_back({ lanes * (L / 2), L, ns, parity, floatBits(sign) });
        parity ^= 1u;
    }
    std::string body = std::string(CHIRP_WGSL) + STOCKHAM_WGSL;
    GpuDispatchKernelSequence(fftPipeline("gpuArray:fft:stockham", body.c_str()), { x, y }, passes,
        groups((uint64_t)lanes * (L / 2)), 1);
    return (parity == 0) ? x : y;
}
//=============================================================================
static uint32_t
nextPow2(uint32_t v)
{
    uint32_t p = 1;
    while (p < v) {
        p <<= 1;
    }
    return p;
}
//=============================================================================
ArrayOf
GpuFftAlong(GpuArrayObject* a, indexType length, int dim, bool inverse)
{
    Dimensions dims = a->dimensions();
    indexType ndims = std::max<indexType>(dims.length(), 2);
    if (dim <= 0) {
        dim = (int)ndims;
        for (indexType k = 0; k < ndims; ++k) {
            if (dims.dimensionLength(k) != 1) {
                dim = (int)k + 1;
                break;
            }
        }
    }
    std::vector<indexType> outLengths;
    for (indexType k = 0; k < std::max<indexType>(ndims, (indexType)dim); ++k) {
        outLengths.push_back(k < ndims ? dims.dimensionLength(k) : 1);
    }
    indexType inLen = outLengths[(size_t)dim - 1];
    outLengths[(size_t)dim - 1] = length;
    Dimensions outDims(outLengths);
    outDims.simplify();
    indexType before = 1;
    for (int k = 0; k < dim - 1; ++k) {
        before *= outLengths[(size_t)k];
    }
    indexType total = outDims.elementCount();
    indexType lanes = (length == 0) ? 0 : total / length;
    GpuArrayObject* output = AllocateGpuArray(outDims, "single", true);
    ArrayOf result = ArrayOf::handle(output);
    if (total == 0) {
        return result;
    }
    uint32_t n = (uint32_t)length;
    float sign = inverse ? 1.0f : -1.0f;
    bool power = (n & (n - 1)) == 0;
    uint32_t L = power ? n : nextPow2(2 * n - 1);
    std::string loadBody = std::string(CHIRP_WGSL) + LOAD_WGSL;
    std::string storeBody = std::string(CHIRP_WGSL) + STORE_WGSL;
    GpuArrayObject* w0 = AllocateGpuArray(Dimensions((indexType)lanes * L, 1), "single", true);
    ArrayOf w0Owner = ArrayOf::handle(w0);
    GpuArrayObject* w1 = AllocateGpuArray(Dimensions((indexType)lanes * L, 1), "single", true);
    ArrayOf w1Owner = ArrayOf::handle(w1);
    uint32_t loadParams[8] = { (uint32_t)(lanes * L), L, (uint32_t)inLen, (uint32_t)before, n,
        a->isComplex() ? 1u : 0u, power ? 0u : 1u, floatBits(sign) };
    GpuDispatchKernel(fftPipeline("gpuArray:fft:load", loadBody.c_str()),
        { a->buffer(), w0->buffer() }, loadParams, 8, groups((uint64_t)lanes * L), 1);
    WGPUBuffer rows = nullptr;
    float scale = inverse ? 1.0f / (float)n : 1.0f;
    if (power) {
        rows = stockham(w0->buffer(), w1->buffer(), (uint32_t)lanes, L, sign);
    } else {
        // Bluestein: chirp-multiplied rows convolved with the conjugate chirp
        GpuArrayObject* f0 = AllocateGpuArray(Dimensions(L, 1), "single", true);
        ArrayOf f0Owner = ArrayOf::handle(f0);
        GpuArrayObject* f1 = AllocateGpuArray(Dimensions(L, 1), "single", true);
        ArrayOf f1Owner = ArrayOf::handle(f1);
        std::string filterBody = std::string(CHIRP_WGSL) + FILTER_WGSL;
        uint32_t filterParams[3] = { L, n, floatBits(sign) };
        GpuDispatchKernel(fftPipeline("gpuArray:fft:filter", filterBody.c_str()),
            { f0->buffer() }, filterParams, 3, groups(L), 1);
        WGPUBuffer filter = stockham(f0->buffer(), f1->buffer(), 1, L, -1.0f);
        WGPUBuffer spectrum = stockham(w0->buffer(), w1->buffer(), (uint32_t)lanes, L, -1.0f);
        std::string multiplyBody = std::string(CHIRP_WGSL) + MULTIPLY_WGSL;
        uint32_t multiplyParams[2] = { (uint32_t)(lanes * L), L };
        GpuDispatchKernel(fftPipeline("gpuArray:fft:multiply", multiplyBody.c_str()),
            { filter, spectrum }, multiplyParams, 2, groups((uint64_t)lanes * L), 1);
        WGPUBuffer other = (spectrum == w0->buffer()) ? w1->buffer() : w0->buffer();
        rows = stockham(spectrum, other, (uint32_t)lanes, L, 1.0f);
        scale /= (float)L;
        uint32_t storeParams[8] = { (uint32_t)total, L, n, (uint32_t)before, floatBits(scale), 1u,
            floatBits(sign), 0u };
        GpuDispatchKernel(fftPipeline("gpuArray:fft:store", storeBody.c_str()),
            { rows, output->buffer() }, storeParams, 8, groups(total), 1);
        return result;
    }
    uint32_t storeParams[8]
        = { (uint32_t)total, L, n, (uint32_t)before, floatBits(scale), 0u, floatBits(sign), 0u };
    GpuDispatchKernel(fftPipeline("gpuArray:fft:store", storeBody.c_str()),
        { rows, output->buffer() }, storeParams, 8, groups(total), 1);
    return result;
}
//=============================================================================
// true when every element equals the conjugate of its mirror across the given dimensions
static const char* SYMMETRY_WGSL = R"WGSL(
struct Params { n : u32, rank : u32, mask : u32, pad : u32, lengths : array<vec4<u32>, 2> };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> flag : array<atomic<u32>>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    var rest = i;
    var mirror = 0u;
    var stride = 1u;
    for (var d = 0u; d < params.rank; d = d + 1u) {
        let len = params.lengths[d / 4u][d % 4u];
        let c = rest % len;
        rest = rest / len;
        var m = c;
        if ((params.mask & (1u << d)) != 0u) { m = (len - c) % len; }
        mirror = mirror + m * stride;
        stride = stride * len;
    }
    if (a[2u * i] != a[2u * mirror] || a[2u * i + 1u] != -a[2u * mirror + 1u]) {
        atomicOr(&flag[0], 1u);
    }
}
)WGSL";
//=============================================================================
bool
GpuIsConjugateSymmetric(GpuArrayObject* a, const std::vector<int>& dims)
{
    if (!a->isComplex()) {
        return false;
    }
    Dimensions d = a->dimensions();
    if (d.length() > 8 || a->elementCount() == 0) {
        return false;
    }
    std::vector<uint32_t> params(12, 1u);
    params[0] = (uint32_t)a->elementCount();
    params[1] = (uint32_t)d.length();
    params[2] = 0;
    params[3] = 0;
    for (int dim : dims) {
        params[2] |= 1u << (dim - 1);
    }
    for (indexType k = 0; k < d.length(); ++k) {
        params[4 + k] = (uint32_t)d.dimensionLength(k);
    }
    GpuArrayObject* flag = AllocateGpuArray(Dimensions(1, 1), "uint32", false);
    ArrayOf owner = ArrayOf::handle(flag);
    WGPUComputePipeline p
        = GpuContext::getInstance()->pipelineFor("gpuArray:fft:symmetry", SYMMETRY_WGSL, "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuDispatchKernel(p, { a->buffer(), flag->buffer() }, params.data(), params.size(),
        groups(a->elementCount()), 1);
    ArrayOf host = GatherGpuArray(flag);
    return ((const uint32_t*)host.dataPointer())[0] == 0;
}
//=============================================================================
// spectrum of real data: the later element of each mirror pair becomes the conjugate of the
// earlier one, self-mirrored elements lose their imaginary part (no two threads touch one slot)
static const char* SYMMETRIZE_WGSL = R"WGSL(
struct Params { n : u32, rank : u32, mask : u32, pad : u32, lengths : array<vec4<u32>, 2> };
@group(0) @binding(0) var<storage, read_write> a : array<f32>;
@group(0) @binding(1) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    var rest = i;
    var mirror = 0u;
    var stride = 1u;
    for (var d = 0u; d < params.rank; d = d + 1u) {
        let len = params.lengths[d / 4u][d % 4u];
        let c = rest % len;
        rest = rest / len;
        var m = c;
        if ((params.mask & (1u << d)) != 0u) { m = (len - c) % len; }
        mirror = mirror + m * stride;
        stride = stride * len;
    }
    if (mirror == i) { a[2u * i + 1u] = 0.0; }
    else if (i > mirror) {
        a[2u * i] = a[2u * mirror];
        a[2u * i + 1u] = -a[2u * mirror + 1u];
    }
}
)WGSL";
//=============================================================================
void
GpuMakeConjugateSymmetric(GpuArrayObject* a, const std::vector<int>& dims)
{
    Dimensions d = a->dimensions();
    if (!a->isComplex() || d.length() > 8 || a->elementCount() == 0) {
        return;
    }
    std::vector<uint32_t> params(12, 1u);
    params[0] = (uint32_t)a->elementCount();
    params[1] = (uint32_t)d.length();
    params[2] = 0;
    params[3] = 0;
    for (int dim : dims) {
        if (dim >= 1 && dim <= (int)d.length()) {
            params[2] |= 1u << (dim - 1);
        }
    }
    for (indexType k = 0; k < d.length(); ++k) {
        params[4 + k] = (uint32_t)d.dimensionLength(k);
    }
    WGPUComputePipeline p = GpuContext::getInstance()->pipelineFor(
        "gpuArray:fft:symmetrize", SYMMETRIZE_WGSL, "main");
    if (p == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuDispatchKernel(
        p, { a->buffer() }, params.data(), params.size(), groups(a->elementCount()), 1);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
