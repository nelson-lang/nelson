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
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static const size_t GPU_REARRANGE_MAX_RANK = 8;
//=============================================================================
// output coordinate c of an axis reads source coordinate ((c / repeat) + offset) % length
static const char* REARRANGE_WGSL = R"WGSL(
struct Params { n : u32, rank : u32, width : u32, pad : u32,
    outLength : array<vec4<u32>, 2>, repeat : array<vec4<u32>, 2>, offset : array<vec4<u32>, 2>,
    length : array<vec4<u32>, 2>, stride : array<vec4<u32>, 2> };
@group(0) @binding(0) var<storage, read> a : array<u32>;
@group(0) @binding(1) var<storage, read_write> c : array<u32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i >= params.n) { return; }
    var rest = i;
    var source = 0u;
    for (var d = 0u; d < params.rank; d = d + 1u) {
        let q = d / 4u;
        let r = d % 4u;
        let coordinate = rest % params.outLength[q][r];
        rest = rest / params.outLength[q][r];
        let s = (coordinate / params.repeat[q][r] + params.offset[q][r]) % params.length[q][r];
        source = source + s * params.stride[q][r];
    }
    for (var w = 0u; w < params.width; w = w + 1u) {
        c[i * params.width + w] = a[source * params.width + w];
    }
}
)WGSL";
//=============================================================================
static std::vector<indexType>
lengthsOf(const Dimensions& dims, size_t rank)
{
    std::vector<indexType> lengths(rank, 1);
    for (size_t k = 0; k < rank && k < (size_t)dims.length(); ++k) {
        lengths[k] = dims.dimensionLength(k);
    }
    return lengths;
}
//=============================================================================
static std::vector<indexType>
stridesOf(const std::vector<indexType>& lengths)
{
    std::vector<indexType> strides(lengths.size(), 1);
    for (size_t k = 1; k < lengths.size(); ++k) {
        strides[k] = strides[k - 1] * lengths[k - 1];
    }
    return strides;
}
//=============================================================================
ArrayOf
GpuRearrange(GpuArrayObject* a, const std::vector<GpuRearrangeAxis>& axes)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (axes.size() > GPU_REARRANGE_MAX_RANK) {
        Error(L"Nelson:gpuArray:rearrangeRankUnsupported",
            L"Rearranging a gpuArray over more than 8 dimensions is not supported.");
    }
    std::vector<indexType> outLengths;
    for (const GpuRearrangeAxis& axis : axes) {
        outLengths.push_back(axis.outLength);
    }
    while (outLengths.size() < 2) {
        outLengths.push_back(1);
    }
    Dimensions outDims(outLengths);
    outDims.simplify();
    GpuArrayObject* output = AllocateGpuArray(outDims, a->underlyingType(), a->isComplex());
    ArrayOf result = ArrayOf::handle(output);
    uint32_t n = (uint32_t)outDims.elementCount();
    if (n == 0 || a->elementCount() == 0) {
        return result;
    }
    const size_t R = GPU_REARRANGE_MAX_RANK;
    std::vector<uint32_t> params(4 + 5 * R, 1u);
    params[0] = n;
    params[1] = (uint32_t)axes.size();
    params[2] = a->isComplex() ? 2u : 1u;
    params[3] = 0u;
    for (size_t k = 0; k < R; ++k) {
        bool used = k < axes.size();
        params[4 + k] = used ? axes[k].outLength : 1u;
        params[4 + R + k] = used ? std::max<uint32_t>(axes[k].repeat, 1u) : 1u;
        params[4 + 2 * R + k] = used ? axes[k].offset : 0u;
        params[4 + 3 * R + k] = used ? std::max<uint32_t>(axes[k].length, 1u) : 1u;
        params[4 + 4 * R + k] = used ? axes[k].stride : 0u;
    }
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor("gpuArray:rearrange", REARRANGE_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params.data(), params.size(),
        (n + 255u) / 256u, 1);
    return result;
}
//=============================================================================
ArrayOf
GpuPermute(GpuArrayObject* a, const std::vector<indexType>& order)
{
    size_t rank = std::max(order.size(), (size_t)a->dimensions().length());
    std::vector<indexType> lengths = lengthsOf(a->dimensions(), rank);
    std::vector<indexType> strides = stridesOf(lengths);
    std::vector<GpuRearrangeAxis> axes(rank);
    for (size_t k = 0; k < rank; ++k) {
        indexType from = (k < order.size()) ? order[k] - 1 : k;
        axes[k].outLength = (uint32_t)lengths[from];
        axes[k].length = (uint32_t)lengths[from];
        axes[k].stride = (uint32_t)strides[from];
    }
    return GpuRearrange(a, axes);
}
//=============================================================================
ArrayOf
GpuRepeat(GpuArrayObject* a, const std::vector<indexType>& elementCopies,
    const std::vector<indexType>& tileCopies)
{
    size_t rank = std::max({ elementCopies.size(), tileCopies.size(),
        (size_t)a->dimensions().length() });
    std::vector<indexType> lengths = lengthsOf(a->dimensions(), rank);
    std::vector<indexType> strides = stridesOf(lengths);
    std::vector<GpuRearrangeAxis> axes(rank);
    for (size_t k = 0; k < rank; ++k) {
        indexType each = (k < elementCopies.size()) ? elementCopies[k] : 1;
        indexType tiles = (k < tileCopies.size()) ? tileCopies[k] : 1;
        axes[k].outLength = (uint32_t)(lengths[k] * each * tiles);
        axes[k].repeat = (uint32_t)std::max<indexType>(each, 1);
        axes[k].length = (uint32_t)lengths[k];
        axes[k].stride = (uint32_t)strides[k];
    }
    return GpuRearrange(a, axes);
}
//=============================================================================
ArrayOf
GpuCircshiftND(GpuArrayObject* a, const std::vector<int64_t>& shifts)
{
    size_t rank = std::max(shifts.size(), (size_t)a->dimensions().length());
    std::vector<indexType> lengths = lengthsOf(a->dimensions(), rank);
    std::vector<indexType> strides = stridesOf(lengths);
    std::vector<GpuRearrangeAxis> axes(rank);
    for (size_t k = 0; k < rank; ++k) {
        int64_t len = (int64_t)std::max<indexType>(lengths[k], 1);
        int64_t shift = (k < shifts.size()) ? shifts[k] % len : 0;
        axes[k].outLength = (uint32_t)lengths[k];
        axes[k].offset = (uint32_t)((len - shift) % len);
        axes[k].length = (uint32_t)len;
        axes[k].stride = (uint32_t)strides[k];
    }
    return GpuRearrange(a, axes);
}
//=============================================================================
ArrayOf
GpuGridAxis(GpuArrayObject* v, const std::vector<indexType>& gridLengths, size_t axis)
{
    std::vector<GpuRearrangeAxis> axes(gridLengths.size());
    for (size_t k = 0; k < gridLengths.size(); ++k) {
        axes[k].outLength = (uint32_t)gridLengths[k];
        axes[k].length = (k == axis) ? (uint32_t)gridLengths[k] : 1u;
        axes[k].stride = (k == axis) ? 1u : 0u;
    }
    return GpuRearrange(v, axes);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
