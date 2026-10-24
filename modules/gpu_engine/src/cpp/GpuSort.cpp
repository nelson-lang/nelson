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
// One thread per lane; in-place selection sort of `vals` (pre-cloned from the
// input) along the sorted dimension, tracking 1-based indices in `idx`.
static const char* SORT_WGSL = R"WGSL(
struct Params { laneCount : u32, redLen : u32, before : u32, descend : u32 };
@group(0) @binding(0) var<storage, read_write> vals : array<f32>;
@group(0) @binding(1) var<storage, read_write> idx : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let lane : u32 = gid.x;
    if (lane >= params.laneCount) {
        return;
    }
    let iB : u32 = lane % params.before;
    let iA : u32 = lane / params.before;
    let start : u32 = iB + iA * (params.before * params.redLen);
    let step : u32 = params.before;
    for (var k : u32 = 0u; k < params.redLen; k = k + 1u) {
        idx[start + k * step] = f32(k + 1u);
    }
    for (var i : u32 = 0u; i + 1u < params.redLen; i = i + 1u) {
        var best : u32 = i;
        for (var j : u32 = i + 1u; j < params.redLen; j = j + 1u) {
            let vj : f32 = vals[start + j * step];
            let vb : f32 = vals[start + best * step];
            var isBetter : bool = vj < vb;
            if (params.descend != 0u) {
                isBetter = vj > vb;
            }
            if (isBetter) {
                best = j;
            }
        }
        if (best != i) {
            let pi : u32 = start + i * step;
            let pb : u32 = start + best * step;
            let tv : f32 = vals[pi];
            vals[pi] = vals[pb];
            vals[pb] = tv;
            let ti : f32 = idx[pi];
            idx[pi] = idx[pb];
            idx[pb] = ti;
        }
    }
}
)WGSL";
// Complex per-lane selection sort by magnitude; vals is interleaved (re, im),
// idx is one f32 per element (1-based).
static const char* SORT_COMPLEX_WGSL = R"WGSL(
struct Params { laneCount : u32, redLen : u32, before : u32, descend : u32 };
@group(0) @binding(0) var<storage, read_write> vals : array<f32>;
@group(0) @binding(1) var<storage, read_write> idx : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let lane : u32 = gid.x;
    if (lane >= params.laneCount) {
        return;
    }
    let iB : u32 = lane % params.before;
    let iA : u32 = lane / params.before;
    let start : u32 = iB + iA * (params.before * params.redLen);
    let step : u32 = params.before;
    for (var k : u32 = 0u; k < params.redLen; k = k + 1u) {
        idx[start + k * step] = f32(k + 1u);
    }
    for (var i : u32 = 0u; i + 1u < params.redLen; i = i + 1u) {
        var best : u32 = i;
        let pbi : u32 = start + i * step;
        var bestMag : f32 = vals[2u * pbi] * vals[2u * pbi] + vals[2u * pbi + 1u] * vals[2u * pbi + 1u];
        for (var j : u32 = i + 1u; j < params.redLen; j = j + 1u) {
            let pj : u32 = start + j * step;
            let vm : f32 = vals[2u * pj] * vals[2u * pj] + vals[2u * pj + 1u] * vals[2u * pj + 1u];
            var better : bool = vm < bestMag;
            if (params.descend != 0u) {
                better = vm > bestMag;
            }
            if (better) {
                best = j;
                bestMag = vm;
            }
        }
        if (best != i) {
            let pi : u32 = start + i * step;
            let pb : u32 = start + best * step;
            let tr : f32 = vals[2u * pi]; vals[2u * pi] = vals[2u * pb]; vals[2u * pb] = tr;
            let tim : f32 = vals[2u * pi + 1u]; vals[2u * pi + 1u] = vals[2u * pb + 1u]; vals[2u * pb + 1u] = tim;
            let ti : f32 = idx[pi]; idx[pi] = idx[pb]; idx[pb] = ti;
        }
    }
}
)WGSL";
// Selection sort for the raw-int storage: identical to SORT_WGSL but the
// comparison bitcasts to i32/u32 (the swaps move the raw f32 bits unchanged).
static std::string
buildIntSortWgsl(const char* scalarType)
{
    std::string T = scalarType;
    return std::string(
               "struct Params { laneCount : u32, redLen : u32, before : u32, descend : u32 };\n"
               "@group(0) @binding(0) var<storage, read_write> vals : array<f32>;\n"
               "@group(0) @binding(1) var<storage, read_write> idx : array<f32>;\n"
               "@group(0) @binding(2) var<uniform> params : Params;\n"
               "@compute @workgroup_size(256)\n"
               "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
               "    let lane : u32 = gid.x;\n"
               "    if (lane >= params.laneCount) { return; }\n"
               "    let iB : u32 = lane % params.before;\n"
               "    let iA : u32 = lane / params.before;\n"
               "    let start : u32 = iB + iA * (params.before * params.redLen);\n"
               "    let step : u32 = params.before;\n"
               "    for (var k : u32 = 0u; k < params.redLen; k = k + 1u) {\n"
               "        idx[start + k * step] = f32(k + 1u);\n    }\n"
               "    for (var i : u32 = 0u; i + 1u < params.redLen; i = i + 1u) {\n"
               "        var best : u32 = i;\n"
               "        for (var j : u32 = i + 1u; j < params.redLen; j = j + 1u) {\n"
               "            let vj : ")
        + T + " = bitcast<" + T + ">(vals[start + j * step]);\n            let vb : " + T
        + " = bitcast<" + T
        + ">(vals[start + best * step]);\n"
          "            var isBetter : bool = vj < vb;\n"
          "            if (params.descend != 0u) { isBetter = vj > vb; }\n"
          "            if (isBetter) { best = j; }\n        }\n"
          "        if (best != i) {\n"
          "            let pi : u32 = start + i * step;\n"
          "            let pb : u32 = start + best * step;\n"
          "            let tv : f32 = vals[pi]; vals[pi] = vals[pb]; vals[pb] = tv;\n"
          "            let ti : f32 = idx[pi]; idx[pi] = idx[pb]; idx[pb] = ti;\n"
          "        }\n    }\n}\n";
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
    bool complex = a->isComplex();
    GpuStorage storage = a->storageKind();
    bool rawInt = (storage != GpuStorage::Float32);

    // sorted values start as a copy of the input (sorted in place on device)
    GpuArrayObject* values = CloneGpuArray(a);
    GpuArrayObject* indices = AllocateGpuArray(dims);

    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        // nothing to reorder along a singleton dimension: indices are all 1
        indexType elementCount = dims.elementCount();
        single* ones = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, elementCount);
        for (indexType k = 0; k < elementCount; ++k) {
            ones[k] = 1.0f;
        }
        retval << ArrayOf::handle(values);
        retval << CreateGpuArray(ArrayOf(NLS_SINGLE, dims, ones));
        delete indices;
        return retval;
    }
    std::string intSortSource;
    std::string key = "gpuArray:sort";
    const char* source = SORT_WGSL;
    if (complex) {
        key = "gpuArray:sort_complex";
        source = SORT_COMPLEX_WGSL;
    } else if (rawInt) {
        intSortSource = buildIntSortWgsl(storage == GpuStorage::Uint32 ? "u32" : "i32");
        key = "gpuArray:sort_i32:" + a->underlyingType();
        source = intSortSource.c_str();
    }
    WGPUComputePipeline pipeline = context->pipelineFor(key, source, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[4] = { lane.laneCount, lane.redLen, lane.before, descend ? 1u : 0u };
    uint32_t workgroupCountX = (lane.laneCount + 255u) / 256u;
    GpuDispatchKernel(
        pipeline, { values->buffer(), indices->buffer() }, params, 4, workgroupCountX, 1);
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
