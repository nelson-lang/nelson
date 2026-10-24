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
// nComp = 1 for real, 2 for complex (interleaved re/im). conj negates the
// imaginary component (complex conjugate transpose).
static const char* TRANSPOSE_WGSL = R"WGSL(
struct Params { m : u32, n : u32, nComp : u32, conj : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> c : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let o = gid.x;
    let total = params.m * params.n;
    if (o < total) {
        // output is n x m (column-major): r in [0,n), col in [0,m)
        let r = o % params.n;
        let col = o / params.n;
        let src = col + r * params.m;
        for (var k : u32 = 0u; k < params.nComp; k = k + 1u) {
            var v = a[src * params.nComp + k];
            if (params.conj != 0u && k == 1u) { v = -v; }
            c[o * params.nComp + k] = v;
        }
    }
}
)WGSL";
//=============================================================================
static const char* GATHER_INDEX_WGSL = R"WGSL(
struct Params { n : u32, nComp : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read> idx : array<u32>;
@group(0) @binding(2) var<storage, read_write> c : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let o = gid.x;
    if (o < params.n) {
        let src = idx[o];
        for (var k : u32 = 0u; k < params.nComp; k = k + 1u) {
            c[o * params.nComp + k] = a[src * params.nComp + k];
        }
    }
}
)WGSL";
//=============================================================================
static ArrayOf
gatherByLinearIndex(
    GpuArrayObject* a, const std::vector<uint32_t>& zeroBasedIndices, const Dimensions& outDims);
//=============================================================================
ArrayOf
GpuTranspose(GpuArrayObject* a, bool conjugate)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    Dimensions dims = a->dimensions();
    if (!dims.is2D()) {
        Error(L"Nelson:transpose:not2D", L"transpose on gpuArray supports 2-D arrays only.");
    }
    uint32_t m = (uint32_t)dims.rows();
    uint32_t n = (uint32_t)dims.columns();
    uint32_t nComp = a->isComplex() ? 2u : 1u;
    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:transpose", TRANSPOSE_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    Dimensions outputDims(n, m);
    GpuArrayObject* output = AllocateGpuArray(outputDims, a->underlyingType(), a->isComplex());
    uint32_t total = m * n;
    if (total > 0) {
        uint32_t params[4] = { m, n, nComp, (conjugate && a->isComplex()) ? 1u : 0u };
        uint32_t workgroupCountX = (total + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 4, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuReshape(GpuArrayObject* a, const Dimensions& newDims)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (newDims.elementCount() != a->dimensions().elementCount()) {
        Error(L"Nelson:reshape:elementCountMismatch",
            L"reshape: the number of elements must not change.");
    }
    GpuArrayObject* output = AllocateGpuArray(newDims, a->underlyingType(), a->isComplex());
    indexType elementCount = newDims.elementCount();
    if (elementCount > 0) {
        size_t byteSize = (size_t)output->floatCount() * sizeof(float);
        WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
        WGPUCommandEncoder encoder
            = wgpuDeviceCreateCommandEncoder(context->device(), &encoderDesc);
        wgpuCommandEncoderCopyBufferToBuffer(
            encoder, a->buffer(), 0, output->buffer(), 0, byteSize);
        WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
        WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
        wgpuQueueSubmit(context->queue(), 1, &commands);
        wgpuCommandBufferRelease(commands);
        wgpuCommandEncoderRelease(encoder);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuIndexLinear(
    GpuArrayObject* a, const std::vector<double>& oneBasedLinearIdx, const Dimensions& outDims)
{
    indexType total = a->dimensions().elementCount();
    std::vector<uint32_t> zeroBased(oneBasedLinearIdx.size());
    for (size_t k = 0; k < oneBasedLinearIdx.size(); ++k) {
        double value = oneBasedLinearIdx[k];
        if (value < 1.0 || value > (double)total || value != (double)(indexType)value) {
            Error(L"Nelson:gpuArray:indexOutOfBound", L"Index exceeds gpuArray dimensions.");
        }
        zeroBased[k] = (uint32_t)((indexType)value - 1);
    }
    return gatherByLinearIndex(a, zeroBased, outDims);
}
//=============================================================================
ArrayOf
GpuIndex2D(GpuArrayObject* a, const std::vector<double>& oneBasedRows,
    const std::vector<double>& oneBasedCols)
{
    Dimensions dims = a->dimensions();
    if (!dims.is2D()) {
        Error(L"Nelson:gpuArray:indexNot2D",
            L"Two-subscript indexing on gpuArray supports 2-D arrays only.");
    }
    indexType rows = dims.rows();
    indexType columns = dims.columns();
    size_t outRows = oneBasedRows.size();
    size_t outCols = oneBasedCols.size();

    std::vector<uint32_t> zeroBased(outRows * outCols);
    for (size_t q = 0; q < outCols; ++q) {
        double cValue = oneBasedCols[q];
        if (cValue < 1.0 || cValue > (double)columns || cValue != (double)(indexType)cValue) {
            Error(L"Nelson:gpuArray:columnIndexOutOfBound",
                L"Column index exceeds gpuArray dimensions.");
        }
        indexType cIndex = (indexType)cValue - 1;
        for (size_t p = 0; p < outRows; ++p) {
            double rValue = oneBasedRows[p];
            if (rValue < 1.0 || rValue > (double)rows || rValue != (double)(indexType)rValue) {
                Error(L"Nelson:gpuArray:rowIndexOutOfBound",
                    L"Row index exceeds gpuArray dimensions.");
            }
            indexType rIndex = (indexType)rValue - 1;
            // column-major output (outRows x outCols) and input (rows x columns)
            zeroBased[p + q * outRows] = (uint32_t)(rIndex + cIndex * rows);
        }
    }
    Dimensions outDims((indexType)outRows, (indexType)outCols);
    return gatherByLinearIndex(a, zeroBased, outDims);
}
//=============================================================================
// Turn N one-based subscript lists into the flat column-major linear indices of
// their cartesian product (subscript 0 varies fastest). Each subscript indexes
// its own dimension against the array's natural strides; dimensions not covered
// by a subscript are implicitly index 1, and extra subscripts must be 1 - the
// same rule the host interpreter applies (it does not fold trailing dimensions).
std::vector<uint32_t>
GpuCartesianLinearIndices(
    const Dimensions& dims, const std::vector<std::vector<double>>& subs, Dimensions& outDims)
{
    Dimensions workingDims(dims);
    indexType rank = (indexType)workingDims.length();
    indexType nbSubs = (indexType)subs.size();
    // effective size along each subscript axis, and the array's natural strides
    std::vector<indexType> effective((size_t)nbSubs, 1);
    std::vector<indexType> strides((size_t)nbSubs, 1);
    indexType runningStride = 1;
    for (indexType i = 0; i < nbSubs; ++i) {
        indexType axisSize = (i < rank) ? (indexType)workingDims[i] : (indexType)1;
        effective[(size_t)i] = axisSize;
        strides[(size_t)i] = runningStride;
        runningStride *= axisSize;
    }
    std::vector<std::vector<uint32_t>> offsets((size_t)nbSubs);
    indexType outCount = 1;
    std::vector<indexType> counts((size_t)nbSubs, 0);
    for (indexType i = 0; i < nbSubs; ++i) {
        const std::vector<double>& list = subs[(size_t)i];
        offsets[(size_t)i].resize(list.size());
        for (size_t k = 0; k < list.size(); ++k) {
            double value = list[k];
            if (value < 1.0 || value > (double)effective[(size_t)i]
                || value != (double)(indexType)value) {
                Error(L"Nelson:gpuArray:indexOutOfBound", L"Index exceeds gpuArray dimensions.");
            }
            offsets[(size_t)i][k] = (uint32_t)(((indexType)value - 1) * strides[(size_t)i]);
        }
        counts[(size_t)i] = (indexType)list.size();
        outCount *= counts[(size_t)i];
    }
    std::vector<uint32_t> linear((size_t)outCount);
    std::vector<size_t> odometer((size_t)nbSubs, 0);
    for (indexType out = 0; out < outCount; ++out) {
        uint32_t position = 0;
        for (indexType i = 0; i < nbSubs; ++i) {
            position += offsets[(size_t)i][odometer[(size_t)i]];
        }
        linear[(size_t)out] = position;
        for (indexType i = 0; i < nbSubs; ++i) {
            if (++odometer[(size_t)i] < (size_t)counts[(size_t)i]) {
                break;
            }
            odometer[(size_t)i] = 0;
        }
    }
    std::vector<indexType> outVec((size_t)nbSubs);
    for (indexType i = 0; i < nbSubs; ++i) {
        outVec[(size_t)i] = counts[(size_t)i];
    }
    outDims = Dimensions(outVec);
    outDims.simplify();
    return linear;
}
//=============================================================================
ArrayOf
GpuIndexND(GpuArrayObject* a, const std::vector<std::vector<double>>& subs)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    Dimensions outDims;
    std::vector<uint32_t> linear = GpuCartesianLinearIndices(a->dimensions(), subs, outDims);
    return gatherByLinearIndex(a, linear, outDims);
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
gatherByLinearIndex(
    GpuArrayObject* a, const std::vector<uint32_t>& zeroBasedIndices, const Dimensions& outDims)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    GpuArrayObject* output = AllocateGpuArray(outDims, a->underlyingType(), a->isComplex());
    uint32_t n = (uint32_t)zeroBasedIndices.size();
    if (n == 0) {
        return ArrayOf::handle(output);
    }
    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:gather_index", GATHER_INDEX_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    WGPUBuffer indexBuffer = GpuCreateIndexBuffer(zeroBasedIndices);
    uint32_t params[2] = { n, a->isComplex() ? 2u : 1u };
    uint32_t workgroupCountX = (n + 255u) / 256u;
    GpuDispatchKernel(
        pipeline, { a->buffer(), indexBuffer, output->buffer() }, params, 2, workgroupCountX, 1);
    // wait until the kernel has consumed the temporary index buffer before release
    context->waitIdle();
    wgpuBufferRelease(indexBuffer);
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
