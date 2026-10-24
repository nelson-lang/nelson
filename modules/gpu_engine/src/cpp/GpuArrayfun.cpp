//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
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
// Build the WGSL for an N-input fused elementwise kernel. Inputs are bound as
// in0..in{N-1} (read), the output as outv (read_write), then a uniform holding
// the element count and one broadcast stride per input.
static std::string
buildArrayfunWgsl(const std::string& wgslExpr, size_t inputCount)
{
    std::string params = "struct Params { n : u32";
    std::string bindings;
    std::string prologue;
    for (size_t k = 0; k < inputCount; ++k) {
        std::string sk = std::to_string(k);
        params += ", s" + sk + " : u32";
        bindings
            += "@group(0) @binding(" + sk + ") var<storage, read> in" + sk + " : array<f32>;\n";
        prologue += "        let p" + sk + " : f32 = in" + sk + "[i * params.s" + sk + "];\n";
    }
    params += " };\n";
    std::string outBinding = "@group(0) @binding(" + std::to_string(inputCount)
        + ") var<storage, read_write> outv : array<f32>;\n";
    std::string uniformBinding = "@group(0) @binding(" + std::to_string(inputCount + 1)
        + ") var<uniform> params : Params;\n";

    return params + bindings + outBinding + uniformBinding
        + "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i < params.n) {\n"
        + prologue + "        outv[i] = " + wgslExpr + ";\n    }\n}\n";
}
//=============================================================================
// Same layout, but each parameter is read as an i32 (small integer stored as an
// exact f32 value) and the saturating-integer expression is stored back as an
// f32 value. The result buffer keeps the integer (or logical) class tag.
static std::string
buildArrayfunIntegerWgsl(const std::string& wgslExpr, size_t inputCount)
{
    std::string params = "struct Params { n : u32";
    std::string bindings;
    std::string prologue;
    for (size_t k = 0; k < inputCount; ++k) {
        std::string sk = std::to_string(k);
        params += ", s" + sk + " : u32";
        bindings
            += "@group(0) @binding(" + sk + ") var<storage, read> in" + sk + " : array<f32>;\n";
        prologue += "        let p" + sk + " : i32 = i32(in" + sk + "[i * params.s" + sk + "]);\n";
    }
    params += " };\n";
    std::string outBinding = "@group(0) @binding(" + std::to_string(inputCount)
        + ") var<storage, read_write> outv : array<f32>;\n";
    std::string uniformBinding = "@group(0) @binding(" + std::to_string(inputCount + 1)
        + ") var<uniform> params : Params;\n";

    return params + bindings + outBinding + uniformBinding
        + "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i < params.n) {\n"
        + prologue + "        outv[i] = f32(" + wgslExpr + ");\n    }\n}\n";
}
//=============================================================================
ArrayOf
GpuArrayfun(
    const std::string& wgslExpr, bool logicalResult, const std::vector<GpuArrayObject*>& inputs)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (inputs.empty()) {
        Error(L"Nelson:arrayfun:noArrayInput", L"arrayfun: at least one array input is required.");
    }
    for (auto* input : inputs) {
        if (input == nullptr || !input->isValid()) {
            Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
        }
        if (input->isComplex()) {
            Error(L"Nelson:gpuArray:complexUnsupported",
                L"This operation is not supported for complex gpuArray yet.");
        }
        if (input->isRawIntStorage()) {
            Error(L"Nelson:gpuArray:int32Unsupported",
                L"This operation is not supported for int32 gpuArray yet.");
        }
    }
    // output shape = the shape of the non-scalar inputs (which must agree);
    // scalar inputs broadcast (read stride 0).
    Dimensions outputDims = inputs[0]->dimensions();
    bool haveNonScalar = false;
    for (auto* input : inputs) {
        if (!input->dimensions().isScalar()) {
            if (!haveNonScalar) {
                outputDims = input->dimensions();
                haveNonScalar = true;
            } else if (!input->dimensions().equals(outputDims)) {
                Error(L"Nelson:arrayfun:sizeMismatch",
                    L"arrayfun: all non-scalar inputs must have the same size.");
            }
        }
    }

    std::string source = buildArrayfunWgsl(wgslExpr, inputs.size());
    std::string key = "gpuArray:arrayfun:" + std::to_string(inputs.size()) + ":" + wgslExpr;
    WGPUComputePipeline pipeline = context->pipelineFor(key, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:arrayfun:kernelCompilationFailed",
            L"arrayfun: cannot compile the generated GPU kernel.");
    }

    GpuArrayObject* output = AllocateGpuArray(outputDims, logicalResult ? "logical" : "single");
    uint32_t elementCount = (uint32_t)outputDims.elementCount();

    std::vector<WGPUBuffer> buffers;
    std::vector<uint32_t> params;
    params.push_back(elementCount);
    for (auto* input : inputs) {
        buffers.push_back(input->buffer());
        params.push_back(input->dimensions().isScalar() ? 0u : 1u);
    }
    buffers.push_back(output->buffer());

    if (elementCount > 0) {
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(pipeline, buffers, params.data(), params.size(), workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuArrayfunInteger(const std::string& wgslExpr, bool logicalResult, const std::string& className,
    const std::vector<GpuArrayObject*>& inputs)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (inputs.empty()) {
        Error(L"Nelson:arrayfun:noArrayInput", L"arrayfun: at least one array input is required.");
    }
    for (auto* input : inputs) {
        if (input == nullptr || !input->isValid()) {
            Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
        }
    }
    Dimensions outputDims = inputs[0]->dimensions();
    bool haveNonScalar = false;
    for (auto* input : inputs) {
        if (!input->dimensions().isScalar()) {
            if (!haveNonScalar) {
                outputDims = input->dimensions();
                haveNonScalar = true;
            } else if (!input->dimensions().equals(outputDims)) {
                Error(L"Nelson:arrayfun:sizeMismatch",
                    L"arrayfun: all non-scalar inputs must have the same size.");
            }
        }
    }

    std::string source = buildArrayfunIntegerWgsl(wgslExpr, inputs.size());
    std::string key = "gpuArray:arrayfun:int:" + std::to_string(inputs.size()) + ":" + wgslExpr;
    WGPUComputePipeline pipeline = context->pipelineFor(key, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:arrayfun:kernelCompilationFailed",
            L"arrayfun: cannot compile the generated GPU kernel.");
    }

    // integer expressions saturate to the class range; a comparison / logical
    // root yields a logical result, everything else keeps the integer class
    GpuArrayObject* output = AllocateGpuArray(outputDims, logicalResult ? "logical" : className);
    uint32_t elementCount = (uint32_t)outputDims.elementCount();

    std::vector<WGPUBuffer> buffers;
    std::vector<uint32_t> params;
    params.push_back(elementCount);
    for (auto* input : inputs) {
        buffers.push_back(input->buffer());
        params.push_back(input->dimensions().isScalar() ? 0u : 1u);
    }
    buffers.push_back(output->buffer());

    if (elementCount > 0) {
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(pipeline, buffers, params.data(), params.size(), workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
