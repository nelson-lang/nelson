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
// Place a part (partRows x cols) into an output (totalRows x cols) at a given
// row offset. Column-major scatter used by vertical concatenation.
static const char* VERTCAT_WGSL = R"WGSL(
struct Params { count : u32, partRows : u32, totalRows : u32, rowOffset : u32, nComp : u32 };
@group(0) @binding(0) var<storage, read> part : array<f32>;
@group(0) @binding(1) var<storage, read_write> out : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let e = gid.x;
    if (e < params.count) {
        let r = e % params.partRows;
        let c = e / params.partRows;
        let dst = (params.rowOffset + r) + c * params.totalRows;
        for (var k : u32 = 0u; k < params.nComp; k = k + 1u) {
            out[dst * params.nComp + k] = part[e * params.nComp + k];
        }
    }
}
)WGSL";
//=============================================================================
static void
checkParts(const std::vector<GpuArrayObject*>& parts);
static std::string
concatResultType(const std::vector<GpuArrayObject*>& parts);
//=============================================================================
ArrayOf
GpuHorzcat(const std::vector<GpuArrayObject*>& parts)
{
    GpuContext* context = GpuContext::getInstance();
    checkParts(parts);
    indexType rows = parts[0]->dimensions().rows();
    indexType totalColumns = 0;
    for (auto* part : parts) {
        if (part->dimensions().rows() != rows) {
            Error(L"Nelson:dimagree:horzcatAllInputsNumberRows",
                L"horzcat: all inputs must have the same number of rows.");
        }
        totalColumns += part->dimensions().columns();
    }
    // column-major layout makes horizontal concatenation a contiguous buffer
    // concatenation: just copy each part's buffer end to end.
    Dimensions outputDims(rows, totalColumns);
    GpuArrayObject* output
        = AllocateGpuArray(outputDims, concatResultType(parts), parts[0]->isComplex());

    WGPUCommandEncoderDescriptor encoderDesc = WGPU_COMMAND_ENCODER_DESCRIPTOR_INIT;
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(context->device(), &encoderDesc);
    uint64_t offsetBytes = 0;
    for (auto* part : parts) {
        size_t partBytes = (size_t)part->floatCount() * sizeof(float);
        if (partBytes > 0) {
            wgpuCommandEncoderCopyBufferToBuffer(
                encoder, part->buffer(), 0, output->buffer(), offsetBytes, partBytes);
            offsetBytes += partBytes;
        }
    }
    WGPUCommandBufferDescriptor commandDesc = WGPU_COMMAND_BUFFER_DESCRIPTOR_INIT;
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &commandDesc);
    wgpuQueueSubmit(context->queue(), 1, &commands);
    wgpuCommandBufferRelease(commands);
    wgpuCommandEncoderRelease(encoder);
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuVertcat(const std::vector<GpuArrayObject*>& parts)
{
    GpuContext* context = GpuContext::getInstance();
    checkParts(parts);
    indexType columns = parts[0]->dimensions().columns();
    indexType totalRows = 0;
    for (auto* part : parts) {
        if (part->dimensions().columns() != columns) {
            Error(L"Nelson:dimagree:vertcatAllInputsNumberColumns",
                L"vertcat: all inputs must have the same number of columns.");
        }
        totalRows += part->dimensions().rows();
    }
    Dimensions outputDims(totalRows, columns);
    GpuArrayObject* output
        = AllocateGpuArray(outputDims, concatResultType(parts), parts[0]->isComplex());
    uint32_t nComp = parts[0]->isComplex() ? 2u : 1u;

    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:vertcat", VERTCAT_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t rowOffset = 0;
    for (auto* part : parts) {
        uint32_t partRows = (uint32_t)part->dimensions().rows();
        uint32_t count = (uint32_t)part->dimensions().elementCount();
        if (count > 0) {
            uint32_t params[5] = { count, partRows, (uint32_t)totalRows, rowOffset, nComp };
            uint32_t workgroupCountX = (count + 255u) / 256u;
            GpuDispatchKernel(
                pipeline, { part->buffer(), output->buffer() }, params, 5, workgroupCountX, 1);
        }
        rowOffset += partRows;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// Column-major scatter of a part into the output at a given offset along the
// concatenation dimension. before = product of dims below `dim`, dimLen = the
// part's size along `dim`, totalDimLen = the output size along `dim`.
static const char* CATND_WGSL = R"WGSL(
struct Params { count : u32, before : u32, dimLen : u32, totalDimLen : u32, dimOffset : u32, nComp : u32 };
@group(0) @binding(0) var<storage, read> part : array<f32>;
@group(0) @binding(1) var<storage, read_write> out : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let e = gid.x;
    if (e < params.count) {
        let iBefore = e % params.before;
        let rest = e / params.before;
        let k = rest % params.dimLen;
        let iAfter = rest / params.dimLen;
        let dst = iBefore + (params.dimOffset + k) * params.before
                + iAfter * (params.before * params.totalDimLen);
        for (var c : u32 = 0u; c < params.nComp; c = c + 1u) {
            out[dst * params.nComp + c] = part[e * params.nComp + c];
        }
    }
}
)WGSL";
//=============================================================================
ArrayOf
GpuCatND(const std::vector<GpuArrayObject*>& parts, int dim)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (parts.empty()) {
        Error(L"Nelson:gpuArray:emptyConcatInput",
            L"gpuArray concatenation requires at least one non-empty input.");
    }
    if (dim < 3) {
        Error(L"Nelson:gpu:invalidDimension", L"GpuCatND expects a dimension >= 3.");
    }
    bool firstComplex = parts[0]->isComplex();
    for (auto* part : parts) {
        if (part == nullptr || !part->isValid()) {
            Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
        }
        if (part->isComplex() != firstComplex) {
            Error(L"Nelson:gpuArray:complexUnsupported",
                L"This operation is not supported for complex gpuArray yet.");
        }
    }
    size_t d = (size_t)(dim - 1); // 0-based concatenation dimension
    // output dimensions: same as part[0] except dim d is the sum of the parts'
    // sizes along d; every other dimension must match
    indexType ndims = (indexType)parts[0]->dimensions().length();
    if ((indexType)dim > ndims) {
        ndims = dim;
    }
    std::vector<indexType> outDims((size_t)ndims, 1);
    for (indexType k = 0; k < (indexType)parts[0]->dimensions().length(); ++k) {
        outDims[(size_t)k] = parts[0]->dimensions().dimensionLength(k);
    }
    indexType totalDimLen = 0;
    for (auto* part : parts) {
        for (indexType k = 0; k < ndims; ++k) {
            if ((size_t)k == d) {
                continue;
            }
            indexType partLen = (k < (indexType)part->dimensions().length())
                ? part->dimensions().dimensionLength(k)
                : 1;
            if (partLen != outDims[(size_t)k]) {
                Error(L"Nelson:dimagree:catDimensionsInputsAreNotConsistent",
                    L"cat: dimensions of the inputs are not consistent.");
            }
        }
        totalDimLen += (d < part->dimensions().length())
            ? part->dimensions().dimensionLength((indexType)d)
            : 1;
    }
    outDims[d] = totalDimLen;
    Dimensions outputDims(outDims);

    uint32_t before = 1;
    for (size_t k = 0; k < d; ++k) {
        before *= (uint32_t)outDims[k];
    }
    uint32_t nComp = parts[0]->isComplex() ? 2u : 1u;
    GpuArrayObject* output
        = AllocateGpuArray(outputDims, concatResultType(parts), parts[0]->isComplex());

    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:catnd", CATND_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t dimOffset = 0;
    for (auto* part : parts) {
        uint32_t dimLen = (d < part->dimensions().length())
            ? (uint32_t)part->dimensions().dimensionLength((indexType)d)
            : 1u;
        uint32_t count = (uint32_t)part->dimensions().elementCount();
        if (count > 0) {
            uint32_t params[6] = { count, before, dimLen, (uint32_t)totalDimLen, dimOffset, nComp };
            uint32_t workgroupCountX = (count + 255u) / 256u;
            GpuDispatchKernel(
                pipeline, { part->buffer(), output->buffer() }, params, 6, workgroupCountX, 1);
        }
        dimOffset += dimLen;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// static helper functions
//=============================================================================
static void
checkParts(const std::vector<GpuArrayObject*>& parts)
{
    if (!GpuContext::getInstance()->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (parts.empty()) {
        Error(L"Nelson:gpuArray:emptyConcatInput",
            L"gpuArray concatenation requires at least one non-empty input.");
    }
    bool firstComplex = parts[0]->isComplex();
    for (auto* part : parts) {
        if (part == nullptr || !part->isValid()) {
            Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
        }
        if (!part->dimensions().is2D()) {
            Error(L"Nelson:gpuArray:concatNot2D",
                L"gpuArray concatenation supports 2-D arrays only.");
        }
        // mixing real and complex parts would require promotion: not yet supported
        if (part->isComplex() != firstComplex) {
            Error(L"Nelson:gpuArray:complexUnsupported",
                L"This operation is not supported for complex gpuArray yet.");
        }
    }
}
//=============================================================================
// Result class of a concatenation: an integer class wins (its values are read
// back in that class), so [int8 single] is int8. Two different integer classes
// cannot be mixed. int32/uint32 use raw-bit storage, so they cannot be mixed
// with any other class (that would reinterpret the bits) and every input must
// be exactly that class.
static std::string
concatResultType(const std::vector<GpuArrayObject*>& parts)
{
    std::string intType;
    bool hasRaw32 = false;
    for (auto* part : parts) {
        std::string ut = part->underlyingType();
        NelsonType hostClass = NLS_SINGLE;
        std::string lo, hi;
        bool isSmallInt = GpuIntegerUnderlyingType(ut, hostClass, lo, hi);
        bool isWideInt = (ut == "int32" || ut == "uint32");
        if (isSmallInt || isWideInt) {
            if (intType.empty()) {
                intType = ut;
            } else if (intType != ut) {
                Error(L"Nelson:gpuArray:integerTypeMismatch", L"Integer types must match.");
            }
            hasRaw32 = hasRaw32 || isWideInt;
        }
    }
    if (hasRaw32) {
        for (auto* part : parts) {
            if (part->underlyingType() != intType) {
                Error(L"Nelson:gpuArray:concatTypeMismatch",
                    L"int32/uint32 gpuArray concatenation requires all inputs of the same class.");
            }
        }
    }
    return intType.empty() ? parts[0]->underlyingType() : intType;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
