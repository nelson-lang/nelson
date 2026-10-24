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
// out[idx[k]] = vals[k * valStride]; valStride 0 broadcasts a scalar RHS.
// nComp = 1 for real, 2 for complex (interleaved re/im).
static const char* SCATTER_WGSL = R"WGSL(
struct Params { n : u32, valStride : u32, nComp : u32 };
@group(0) @binding(0) var<storage, read> vals : array<f32>;
@group(0) @binding(1) var<storage, read> idx : array<u32>;
@group(0) @binding(2) var<storage, read_write> out : array<f32>;
@group(0) @binding(3) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let k = gid.x;
    if (k < params.n) {
        let src = k * params.valStride;
        for (var c : u32 = 0u; c < params.nComp; c = c + 1u) {
            out[idx[k] * params.nComp + c] = vals[src * params.nComp + c];
        }
    }
}
)WGSL";
//=============================================================================
static ArrayOf
scatterAssign(
    GpuArrayObject* a, const std::vector<uint32_t>& zeroBasedIndices, GpuArrayObject* values);
//=============================================================================
ArrayOf
GpuSubsasgnLinear(GpuArrayObject* a, const std::vector<double>& oneBasedIdx, GpuArrayObject* values)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    indexType total = a->dimensions().elementCount();
    std::vector<uint32_t> zeroBased(oneBasedIdx.size());
    for (size_t k = 0; k < oneBasedIdx.size(); ++k) {
        double value = oneBasedIdx[k];
        if (value < 1.0 || value > (double)total || value != (double)(indexType)value) {
            Error(L"Nelson:gpuArray:indexOutOfBound", L"Index exceeds gpuArray dimensions.");
        }
        zeroBased[k] = (uint32_t)((indexType)value - 1);
    }
    return scatterAssign(a, zeroBased, values);
}
//=============================================================================
ArrayOf
GpuSubsasgn2D(GpuArrayObject* a, const std::vector<double>& oneBasedRows,
    const std::vector<double>& oneBasedCols, GpuArrayObject* values)
{
    Dimensions dims = a->dimensions();
    if (!dims.is2D()) {
        Error(L"Nelson:gpuArray:assignNot2D",
            L"Two-subscript assignment on gpuArray supports 2-D arrays only.");
    }
    indexType rows = dims.rows();
    indexType columns = dims.columns();
    std::vector<uint32_t> zeroBased(oneBasedRows.size() * oneBasedCols.size());
    size_t pos = 0;
    for (size_t q = 0; q < oneBasedCols.size(); ++q) {
        double cValue = oneBasedCols[q];
        if (cValue < 1.0 || cValue > (double)columns || cValue != (double)(indexType)cValue) {
            Error(L"Nelson:gpuArray:columnIndexOutOfBound",
                L"Column index exceeds gpuArray dimensions.");
        }
        indexType cIndex = (indexType)cValue - 1;
        for (size_t p = 0; p < oneBasedRows.size(); ++p) {
            double rValue = oneBasedRows[p];
            if (rValue < 1.0 || rValue > (double)rows || rValue != (double)(indexType)rValue) {
                Error(L"Nelson:gpuArray:rowIndexOutOfBound",
                    L"Row index exceeds gpuArray dimensions.");
            }
            zeroBased[pos++] = (uint32_t)(((indexType)rValue - 1) + cIndex * rows);
        }
    }
    return scatterAssign(a, zeroBased, values);
}
//=============================================================================
ArrayOf
GpuSubsasgnND(
    GpuArrayObject* a, const std::vector<std::vector<double>>& subs, GpuArrayObject* values)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    Dimensions outDims;
    std::vector<uint32_t> zeroBased = GpuCartesianLinearIndices(a->dimensions(), subs, outDims);
    return scatterAssign(a, zeroBased, values);
}
//=============================================================================
// static helper functions
//=============================================================================
static ArrayOf
scatterAssign(
    GpuArrayObject* a, const std::vector<uint32_t>& zeroBasedIndices, GpuArrayObject* values)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    // a and the right-hand side must agree on real/complex (promotion of a
    // mixed assignment is not supported yet)
    bool complex = a->isComplex();
    if ((values != nullptr && values->isComplex()) != complex) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    uint32_t nComp = complex ? 2u : 1u;
    uint32_t n = (uint32_t)zeroBasedIndices.size();
    indexType valueCount = values->dimensions().elementCount();
    uint32_t valStride = 1;
    if (valueCount == 1) {
        valStride = 0; // scalar broadcast
    } else if ((uint32_t)valueCount != n) {
        Error(L"Nelson:gpuArray:assignSizeMismatch",
            L"Assignment: the number of right-hand-side elements does not match the indexed "
            L"region.");
    }
    // value semantics: assign into a fresh copy, leaving the input untouched
    GpuArrayObject* output = CloneGpuArray(a);
    if (n == 0) {
        return ArrayOf::handle(output);
    }
    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:scatter", SCATTER_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    WGPUBuffer indexBuffer = GpuCreateIndexBuffer(zeroBasedIndices);
    uint32_t params[3] = { n, valStride, nComp };
    uint32_t workgroupCountX = (n + 255u) / 256u;
    GpuDispatchKernel(pipeline, { values->buffer(), indexBuffer, output->buffer() }, params, 3,
        workgroupCountX, 1);
    context->waitIdle();
    wgpuBufferRelease(indexBuffer);
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
