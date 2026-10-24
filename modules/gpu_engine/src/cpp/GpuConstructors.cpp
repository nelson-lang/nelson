//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Device-side array constructors (zeros/ones): allocate a storage buffer and
// fill it with a constant on the device, so no host array is created or copied.
//=============================================================================
#include <cstring>
#include <string>
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
ArrayOf
GpuFilled(const Dimensions& dims, float value, const std::string& underlyingType)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, underlyingType, false);
    uint32_t elementCount = (uint32_t)dims.elementCount();
    if (elementCount == 0) {
        return ArrayOf::handle(output);
    }
    // The fill value is passed as a uniform (its bit pattern) and bitcast at
    // runtime: this keeps a single pipeline and avoids the WGSL const-eval that
    // rejects a baked NaN/Inf literal. int32/uint32 store the raw integer bits,
    // every other class stores the f32 value bits.
    uint32_t valueBits = 0;
    if (underlyingType == "int32") {
        int32_t iv = (int32_t)value;
        std::memcpy(&valueBits, &iv, sizeof(int32_t));
    } else if (underlyingType == "uint32") {
        valueBits = (uint32_t)value;
    } else {
        std::memcpy(&valueBits, &value, sizeof(float));
    }
    std::string source = std::string(
        "struct Params { n : u32, value : u32 };\n"
        "@group(0) @binding(0) var<storage, read_write> c : array<f32>;\n"
        "@group(0) @binding(1) var<uniform> params : Params;\n"
        "@compute @workgroup_size(256)\n"
        "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
        "    let i = gid.x;\n"
        "    if (i < params.n) {\n        c[i] = bitcast<f32>(params.value);\n    }\n}\n");
    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:fill", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[2] = { elementCount, valueBits };
    uint32_t workgroupCountX = (elementCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { output->buffer() }, params, 2, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
// Global stream counter: advanced by an odd increment each call so repeated
// rand/randn calls produce different values (device streams, not reproducible
// against the host RNG).
static uint32_t g_randStream = 0x1234567u;
//=============================================================================
ArrayOf
GpuRandom(const Dimensions& dims, bool normal)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, "single", false);
    uint32_t elementCount = (uint32_t)dims.elementCount();
    if (elementCount == 0) {
        return ArrayOf::handle(output);
    }
    g_randStream += 0x9E3779B9u;
    uint32_t seed = g_randStream;

    // PCG-style integer hash -> a well-distributed u32 from a counter
    const char* pcg
        = "fn pcg(v : u32) -> u32 {\n"
          "    var state : u32 = v * 747796405u + 2891336453u;\n"
          "    var word : u32 = ((state >> ((state >> 28u) + 4u)) ^ state) * 277803737u;\n"
          "    return (word >> 22u) ^ word;\n"
          "}\n";
    std::string body;
    if (normal) {
        // Box-Muller from two uniforms; u1 kept away from 0 to avoid log(0)
        body = "        let h1 = pcg(2u * i + params.seed);\n"
               "        let h2 = pcg(2u * i + 1u + params.seed);\n"
               "        let u1 = max(f32(h1) / 4294967296.0, 1e-7);\n"
               "        let u2 = f32(h2) / 4294967296.0;\n"
               "        c[i] = sqrt(-2.0 * log(u1)) * cos(6.28318530718 * u2);\n";
    } else {
        body = "        let h1 = pcg(i + params.seed);\n"
               "        c[i] = f32(h1) / 4294967296.0;\n";
    }
    std::string source = std::string("struct Params { n : u32, seed : u32 };\n") + pcg
        + "@group(0) @binding(0) var<storage, read_write> c : array<f32>;\n"
          "@group(0) @binding(1) var<uniform> params : Params;\n"
          "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n    if (i < params.n) {\n"
        + body + "    }\n}\n";
    WGPUComputePipeline pipeline
        = context->pipelineFor(normal ? "gpuArray:randn" : "gpuArray:rand", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[2] = { elementCount, seed };
    uint32_t workgroupCountX = (elementCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { output->buffer() }, params, 2, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuEye(const Dimensions& dims, const std::string& underlyingType)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, underlyingType, false);
    uint32_t elementCount = (uint32_t)dims.elementCount();
    if (elementCount == 0) {
        return ArrayOf::handle(output);
    }
    uint32_t rows = (uint32_t)dims.dimensionLength(0);
    // int32/uint32 store raw integer bits (1 / 0); every other class stores the
    // f32 value, which gather casts to the target class
    bool rawInt = (underlyingType == "int32" || underlyingType == "uint32");
    std::string diag = rawInt ? "bitcast<f32>(u32(row == col))" : "f32(row == col)";
    // column-major linear index i -> (row = i % rows, col = i / rows); 1 on the
    // main diagonal, 0 elsewhere
    std::string source
        = std::string("struct Params { n : u32, rows : u32 };\n"
                      "@group(0) @binding(0) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(1) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n    if (i < params.n) {\n"
                      "        let row = i % params.rows;\n"
                      "        let col = i / params.rows;\n"
                      "        c[i] = ")
        + diag + ";\n    }\n}\n";
    std::string opKey = std::string("gpuArray:eye:") + (rawInt ? "i32" : "f32");
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[2] = { elementCount, rows };
    uint32_t workgroupCountX = (elementCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { output->buffer() }, params, 2, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuAffineSequence(indexType n, float base, float inc, bool forceLast, float lastValue)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (n < 0) {
        n = 0;
    }
    GpuArrayObject* output = AllocateGpuArray(Dimensions(n == 0 ? 0 : 1, n), "single", false);
    if (n == 0) {
        return ArrayOf::handle(output);
    }
    uint32_t baseBits = 0;
    uint32_t incBits = 0;
    uint32_t lastBits = 0;
    std::memcpy(&baseBits, &base, sizeof(float));
    std::memcpy(&incBits, &inc, sizeof(float));
    std::memcpy(&lastBits, &lastValue, sizeof(float));
    std::string source
        = std::string("struct Params { n : u32, base : u32, inc : u32, last : u32, forceLast : u32 "
                      "};\n"
                      "@group(0) @binding(0) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(1) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n    if (i < params.n) {\n"
                      "        if (params.forceLast == 1u && i == params.n - 1u) {\n"
                      "            c[i] = bitcast<f32>(params.last);\n"
                      "        } else {\n"
                      "            c[i] = bitcast<f32>(params.base) + f32(i) * "
                      "bitcast<f32>(params.inc);\n        }\n    }\n}\n");
    WGPUComputePipeline pipeline = context->pipelineFor("gpuArray:affine", source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t elementCount = (uint32_t)n;
    uint32_t params[5] = { elementCount, baseBits, incBits, lastBits, forceLast ? 1u : 0u };
    uint32_t workgroupCountX = (elementCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { output->buffer() }, params, 5, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
