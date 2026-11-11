//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "GpuHistogram.hpp"
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
// bin of every value: [e(k), e(k+1)) with the last edge included; mode 0 counts, 1 indexes
static const char* BIN_WGSL = R"WGSL(
struct Params { n : u32, edges : u32, mode : u32, pad : u32 };
@group(0) @binding(0) var<storage, read> x : array<f32>;
@group(0) @binding(1) var<storage, read> e : array<f32>;
@group(0) @binding(2) var<storage, read_write> counts : array<atomic<u32>>;
@group(0) @binding(3) var<storage, read_write> index : array<f32>;
@group(0) @binding(4) var<uniform> params : Params;
fn isNan(t : f32) -> bool { return (bitcast<u32>(t) & 0x7fffffffu) > 0x7f800000u; }
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    let v = x[i];
    let last = params.edges - 1u;
    var bin = 0xffffffffu;
    if (!isNan(v) && v >= e[0] && v <= e[last]) {
        if (v == e[last]) { bin = last - 1u; }
        else {
            var lo = 0u;
            var hi = last;
            loop {
                if (hi - lo <= 1u) { break; }
                let mid = (lo + hi) / 2u;
                if (e[mid] <= v) { lo = mid; } else { hi = mid; }
            }
            bin = lo;
        }
    }
    if (params.mode == 0u) {
        if (bin != 0xffffffffu) { atomicAdd(&counts[bin], 1u); }
    } else {
        var nanBits : u32 = 0x7fc00000u;
        index[i] = select(f32(bin + 1u), bitcast<f32>(nanBits), bin == 0xffffffffu);
    }
}
)WGSL";
//=============================================================================
static bool
ascendingEdges(GpuArrayObject* edges)
{
    ArrayOf host = GatherGpuArray(edges);
    const single* p = (const single*)host.dataPointer();
    for (indexType k = 0; k + 1 < host.elementCount(); ++k) {
        if (!(p[k] < p[k + 1])) {
            return false;
        }
    }
    return host.elementCount() >= 2;
}
//=============================================================================
static bool
realSingle(GpuArrayObject* a)
{
    return a != nullptr && !a->isComplex() && !a->isRawIntStorage()
        && GpuIntegerResultType(a, a).empty();
}
//=============================================================================
static void
runBins(
    GpuArrayObject* x, GpuArrayObject* edges, uint32_t mode, WGPUBuffer counts, WGPUBuffer index)
{
    uint32_t n = (uint32_t)x->elementCount();
    if (n == 0) {
        return;
    }
    uint32_t params[4] = { n, (uint32_t)edges->elementCount(), mode, 0u };
    GpuDispatchKernel(pipeline("gpuArray:bins", BIN_WGSL),
        { x->buffer(), edges->buffer(), counts, index }, params, 4, (n + 255u) / 256u, 1);
}
//=============================================================================
bool
GpuHistcounts(GpuArrayObject* x, GpuArrayObject* edges, ArrayOf& result)
{
    if (!realSingle(x) || !realSingle(edges) || !ascendingEdges(edges)) {
        return false;
    }
    indexType bins = edges->elementCount() - 1;
    GpuArrayObject* counts = AllocateGpuArray(Dimensions(1, bins), "uint32", false);
    ArrayOf countsOwner = ArrayOf::handle(counts);
    GpuArrayObject* unused = AllocateGpuArray(Dimensions(1, 1), "single", false);
    ArrayOf unusedOwner = ArrayOf::handle(unused);
    runBins(x, edges, 0u, counts->buffer(), unused->buffer());
    result = GpuCast(countsOwner, "single");
    return true;
}
//=============================================================================
bool
GpuDiscretize(GpuArrayObject* x, GpuArrayObject* edges, ArrayOf& result)
{
    if (!realSingle(x) || !realSingle(edges) || !ascendingEdges(edges)) {
        return false;
    }
    GpuArrayObject* counts = AllocateGpuArray(Dimensions(1, 1), "uint32", false);
    ArrayOf countsOwner = ArrayOf::handle(counts);
    GpuArrayObject* output = AllocateGpuArray(x->dimensions(), "single", false);
    result = ArrayOf::handle(output);
    runBins(x, edges, 1u, counts->buffer(), output->buffer());
    return true;
}
//=============================================================================
// one value per row of subs into the output slot it names; op 0 sum, 1 max, 2 min
static const char* ACCUMULATE_WGSL = R"WGSL(
struct Params { n : u32, k : u32, op : u32, scalar : u32, strides : array<vec4<u32>, 2> };
@group(0) @binding(0) var<storage, read> subs : array<f32>;
@group(0) @binding(1) var<storage, read> vals : array<f32>;
@group(0) @binding(2) var<storage, read_write> acc : array<atomic<u32>>;
@group(0) @binding(3) var<storage, read_write> seen : array<atomic<u32>>;
@group(0) @binding(4) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let r = gid.x;
    if (r >= params.n) { return; }
    var slot = 0u;
    for (var d = 0u; d < params.k; d = d + 1u) {
        slot = slot + (u32(subs[r + d * params.n]) - 1u) * params.strides[d / 4u][d % 4u];
    }
    var v = vals[0];
    if (params.scalar == 0u) { v = vals[r]; }
    atomicOr(&seen[slot], 1u);
    loop {
        let old = atomicLoad(&acc[slot]);
        let current = bitcast<f32>(old);
        var next = current + v;
        if (params.op == 1u) { next = max(current, v); }
        if (params.op == 2u) { next = min(current, v); }
        let swapped = atomicCompareExchangeWeak(&acc[slot], old, bitcast<u32>(next));
        if (swapped.exchanged) { break; }
    }
}
)WGSL";
//=============================================================================
// slots that received nothing get the fill value
static const char* FILL_WGSL = R"WGSL(
struct Params { n : u32, fill : u32 };
@group(0) @binding(0) var<storage, read> seen : array<u32>;
@group(0) @binding(1) var<storage, read_write> acc : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    if (seen[i] == 0u) { acc[i] = bitcast<f32>(params.fill); }
}
)WGSL";
//=============================================================================
bool
GpuAccumarray(GpuArrayObject* subs, GpuArrayObject* vals, const std::vector<indexType>& outDims,
    int op, float fill, ArrayOf& result)
{
    if (!realSingle(subs) || !realSingle(vals) || outDims.empty() || outDims.size() > 8) {
        return false;
    }
    Dimensions sd = subs->dimensions();
    indexType n = sd.rows();
    indexType k = sd.columns();
    if (sd.length() > 2 || (indexType)outDims.size() < k
        || (vals->elementCount() != n && vals->elementCount() != 1)) {
        return false;
    }
    Dimensions dims(outDims);
    if (dims.length() == 1) {
        dims = Dimensions(outDims[0], 1);
    }
    GpuArrayObject* output = AllocateGpuArray(dims, "single", false);
    result = ArrayOf::handle(output);
    uint32_t total = (uint32_t)dims.elementCount();
    if (total == 0) {
        return true;
    }
    // start from the identity of the operation
    float start = (op == 1) ? -INFINITY : (op == 2 ? INFINITY : 0.0f);
    result = GpuFilled(dims, start, "single");
    output = AsGpuArrayObject(result);
    GpuArrayObject* seen = AllocateGpuArray(dims, "uint32", false);
    ArrayOf seenOwner = ArrayOf::handle(seen);
    std::vector<uint32_t> params(12, 0u);
    params[0] = (uint32_t)n;
    params[1] = (uint32_t)k;
    params[2] = (uint32_t)op;
    params[3] = vals->elementCount() == 1 && n != 1 ? 1u : 0u;
    uint32_t stride = 1;
    for (size_t d = 0; d < 8; ++d) {
        params[4 + d] = stride;
        stride *= (d < outDims.size()) ? (uint32_t)outDims[d] : 1u;
    }
    if (n > 0) {
        GpuDispatchKernel(pipeline("gpuArray:accumulate", ACCUMULATE_WGSL),
            { subs->buffer(), vals->buffer(), output->buffer(), seen->buffer() }, params.data(),
            params.size(), (uint32_t)((n + 255) / 256), 1);
    }
    uint32_t fillBits;
    std::memcpy(&fillBits, &fill, sizeof(fillBits));
    uint32_t fillParams[2] = { total, fillBits };
    GpuDispatchKernel(pipeline("gpuArray:accumulatefill", FILL_WGSL),
        { seen->buffer(), output->buffer() }, fillParams, 2, (total + 255u) / 256u, 1);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
