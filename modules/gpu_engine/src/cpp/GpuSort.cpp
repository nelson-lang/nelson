//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
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
// stable order of two elements: value, then NaN placement, then original position
static std::string
precedesWgsl(GpuStorage storage, bool complex)
{
    std::string s = "fn isNan(x : f32) -> bool {\n"
                    "    return (bitcast<u32>(x) & 0x7fffffffu) > 0x7f800000u;\n"
                    "}\n"
                    "fn byPosition(pa : u32, pb : u32) -> bool {\n"
                    "    return bitcast<u32>(idx[pa]) < bitcast<u32>(idx[pb]);\n"
                    "}\n"
                    "fn precedes(pa : u32, pb : u32) -> bool {\n";
    if (storage != GpuStorage::Float32) {
        std::string T = (storage == GpuStorage::Uint32) ? "u32" : "i32";
        return s + "    let x = bitcast<" + T + ">(vals[pa]);\n    let y = bitcast<" + T
            + ">(vals[pb]);\n"
              "    if (x != y) { if (params.descend != 0u) { return x > y; } return x < y; }\n"
              "    return byPosition(pa, pb);\n}\n";
    }
    // NaN goes last in ascending order and first in descending order
    std::string nanRule
        = "    if (nx != ny) { if (params.descend != 0u) { return nx; } return ny; }\n";
    if (complex) {
        // by magnitude, then by angle
        return s
            + "    let xr = vals[2u * pa]; let xi = vals[2u * pa + 1u];\n"
              "    let yr = vals[2u * pb]; let yi = vals[2u * pb + 1u];\n"
              "    let x = xr * xr + xi * xi;\n    let y = yr * yr + yi * yi;\n"
              "    let nx = isNan(x);\n    let ny = isNan(y);\n"
            + nanRule
            + "    if (!nx) {\n"
              "        if (x != y) { if (params.descend != 0u) { return x > y; } return x < y; }\n"
              "        let gx = atan2(xi, xr);\n        let gy = atan2(yi, yr);\n"
              "        if (gx != gy) { if (params.descend != 0u) { return gx > gy; } return gx < "
              "gy; }\n"
              "    }\n    return byPosition(pa, pb);\n}\n";
    }
    return s
        + "    let x = vals[pa];\n    let y = vals[pb];\n"
          "    let nx = isNan(x);\n    let ny = isNan(y);\n"
        + nanRule
        + "    if (!nx && x != y) { if (params.descend != 0u) { return x > y; } return x < y; }\n"
          "    return byPosition(pa, pb);\n}\n";
}
//=============================================================================
// one compare-exchange per thread for one step of a bitonic sort along every lane
static std::string
buildBitonicStepWgsl(GpuStorage storage, bool complex)
{
    std::string swap = complex
        ? "        let tr = vals[2u * pi]; vals[2u * pi] = vals[2u * pl]; vals[2u * pl] = tr;\n"
          "        let ti = vals[2u * pi + 1u]; vals[2u * pi + 1u] = vals[2u * pl + 1u];\n"
          "        vals[2u * pl + 1u] = ti;\n"
        : "        let tv = vals[pi]; vals[pi] = vals[pl]; vals[pl] = tv;\n";
    return std::string("struct Params { pairCount : u32, redLen : u32, before : u32, half : u32,\n"
                       "    block : u32, dist : u32, flip : u32, descend : u32 };\n"
                       "@group(0) @binding(0) var<storage, read_write> vals : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read_write> idx : array<f32>;\n"
                       "@group(0) @binding(2) var<uniform> params : Params;\n")
        + precedesWgsl(storage, complex)
        + "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let t = gid.x;\n"
          "    if (t >= params.pairCount) { return; }\n"
          "    let lane = t / params.half;\n"
          "    let p = t % params.half;\n"
          "    var i : u32;\n"
          "    var l : u32;\n"
          "    if (params.flip != 0u) {\n"
          "        let h = params.block / 2u;\n"
          "        let first = (p / h) * params.block;\n"
          "        i = first + p % h;\n"
          "        l = first + params.block - 1u - p % h;\n"
          "    } else {\n"
          "        i = 2u * params.dist * (p / params.dist) + p % params.dist;\n"
          "        l = i + params.dist;\n"
          "    }\n"
          // positions past the lane end act as +infinity and never move
          "    if (l >= params.redLen) { return; }\n"
          "    let iB = lane % params.before;\n"
          "    let iA = lane / params.before;\n"
          "    let start = iB + iA * (params.before * params.redLen);\n"
          "    let pi = start + i * params.before;\n"
          "    let pl = start + l * params.before;\n"
          "    if (precedes(pl, pi)) {\n"
        + swap
        + "        let tk = idx[pi]; idx[pi] = idx[pl]; idx[pl] = tk;\n"
          "    }\n}\n";
}
//=============================================================================
// idx holds the lane position as raw u32 bits while sorting, then 1-based f32
static const char* SORT_INDEX_INIT_WGSL = R"WGSL(
struct Params { n : u32, redLen : u32, before : u32 };
@group(0) @binding(0) var<storage, read_write> idx : array<f32>;
@group(0) @binding(1) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i < params.n) {
        idx[i] = bitcast<f32>((i / params.before) % params.redLen);
    }
}
)WGSL";
//=============================================================================
static const char* SORT_INDEX_FINISH_WGSL = R"WGSL(
struct Params { n : u32 };
@group(0) @binding(0) var<storage, read_write> idx : array<f32>;
@group(0) @binding(1) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i < params.n) {
        idx[i] = f32(bitcast<u32>(idx[i]) + 1u);
    }
}
)WGSL";
//=============================================================================
static WGPUComputePipeline
sortPipeline(const std::string& key, const char* source)
{
    WGPUComputePipeline pipeline = GpuContext::getInstance()->pipelineFor(key, source, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    return pipeline;
}
//=============================================================================
static void
bitonicSort(GpuArrayObject* values, GpuArrayObject* indices, const GpuLaneGeometry& lane,
    GpuStorage storage, bool complex, bool descend)
{
    uint32_t n = (uint32_t)values->elementCount();
    uint32_t workgroups = (n + 255u) / 256u;
    uint32_t initParams[3] = { n, lane.redLen, lane.before };
    GpuDispatchKernel(sortPipeline("gpuArray:sort:index", SORT_INDEX_INIT_WGSL),
        { indices->buffer() }, initParams, 3, workgroups, 1);

    uint32_t padded = 1;
    while (padded < lane.redLen) {
        padded <<= 1;
    }
    uint32_t half = padded / 2u;
    uint32_t pairCount = lane.laneCount * half;
    std::vector<std::vector<uint32_t>> steps;
    // the first step of each merge compares mirrored pairs so every step sorts upward
    for (uint32_t block = 2; block <= padded; block <<= 1) {
        steps.push_back({ pairCount, lane.redLen, lane.before, half, block, block / 2u, 1u,
            descend ? 1u : 0u });
        for (uint32_t dist = block / 4u; dist >= 1u; dist >>= 1) {
            steps.push_back(
                { pairCount, lane.redLen, lane.before, half, block, dist, 0u, descend ? 1u : 0u });
        }
    }
    std::string kind = complex
        ? "complex"
        : (storage == GpuStorage::Float32 ? "real"
                                          : (storage == GpuStorage::Uint32 ? "u32" : "i32"));
    std::string stepSource = buildBitonicStepWgsl(storage, complex);
    GpuDispatchKernelSequence(sortPipeline("gpuArray:sort:step:" + kind, stepSource.c_str()),
        { values->buffer(), indices->buffer() }, steps, (pairCount + 255u) / 256u, 1);

    uint32_t finishParams[1] = { n };
    GpuDispatchKernel(sortPipeline("gpuArray:sort:indexfinish", SORT_INDEX_FINISH_WGSL),
        { indices->buffer() }, finishParams, 1, workgroups, 1);
}
//=============================================================================
ArrayOfVector
GpuSort(GpuArrayObject* a, int dim, bool descend)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    Dimensions dims = a->dimensions();
    ArrayOfVector retval;
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);

    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        // nothing to reorder along a singleton dimension: indices are all 1
        indexType elementCount = dims.elementCount();
        single* ones = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        for (indexType k = 0; k < elementCount; ++k) {
            ones[k] = 1.0f;
        }
        retval << ArrayOf::handle(CloneGpuArray(a));
        retval << CreateGpuArray(ArrayOf(NLS_SINGLE, dims, ones));
        return retval;
    }
    // sorted values start as a copy of the input (sorted in place on device)
    GpuArrayObject* values = CloneGpuArray(a);
    GpuArrayObject* indices = nullptr;
    try {
        indices = AllocateGpuArray(dims);
        bitonicSort(values, indices, lane, a->storageKind(), a->isComplex(), descend);
    } catch (...) {
        delete values;
        delete indices;
        throw;
    }
    retval << ArrayOf::handle(values);
    retval << ArrayOf::handle(indices);
    return retval;
}
//=============================================================================
ArrayOf
GpuFind(GpuArrayObject* a)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    // find is not a natural data-parallel primitive (variable-size output):
    // read back the values and collect the non-zero linear positions on host,
    // then upload the resulting index list back to the device.
    ArrayOf host = GatherGpuArray(a);
    bool complex = a->isComplex();
    // gather may return a logical or integer class (int8/16/int32); read the
    // magnitudes as single so the non-zero test below is class-agnostic (the
    // != 0 test is exact for any integer, precision loss cannot turn a non-zero
    // value into zero). Complex stays interleaved (re, im) single.
    if (!complex) {
        host.promoteType(NLS_SINGLE);
    }
    Dimensions dims = host.dimensions();
    indexType elementCount = dims.elementCount();
    const single* values = (const single*)host.dataPointer();

    std::vector<single> found;
    for (indexType k = 0; k < elementCount; ++k) {
        // a complex element is non-zero when its real or imaginary part is
        bool nonzero
            = complex ? (values[2 * k] != 0.0f || values[2 * k + 1] != 0.0f) : (values[k] != 0.0f);
        if (nonzero) {
            found.push_back((single)(k + 1));
        }
    }
    // orientation: row vector input -> row result, else column
    bool rowInput = (dims.rows() == 1 && dims.length() <= 2);
    Dimensions outDims = rowInput ? Dimensions(1, (indexType)found.size())
                                  : Dimensions((indexType)found.size(), 1);
    single* destination = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, (indexType)found.size());
    for (size_t k = 0; k < found.size(); ++k) {
        destination[k] = found[k];
    }
    ArrayOf indicesHost(NLS_SINGLE, outDims, destination);
    return CreateGpuArray(indicesHost);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
