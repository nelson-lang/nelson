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
// One thread per lane; sequential prefix scan writing the running value at
// each position along the scanned dimension (column-major before/step layout).
static std::string
buildScanWgsl(const std::string& combineExpr)
{
    return std::string("struct Params { laneCount : u32, redLen : u32, before : u32 };\n"
                       "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                       "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                       "@group(0) @binding(2) var<uniform> params : Params;\n"
                       "@compute @workgroup_size(256)\n"
                       "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                       "    let lane = gid.x;\n"
                       "    if (lane < params.laneCount) {\n"
                       "        let iB = lane % params.before;\n"
                       "        let iA = lane / params.before;\n"
                       "        let start = iB + iA * (params.before * params.redLen);\n"
                       "        var acc : f32 = a[start];\n"
                       "        c[start] = acc;\n"
                       "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
                       "            let idx = start + k * params.before;\n"
                       "            let v = a[idx];\n"
                       "            acc = ")
        + combineExpr
        + ";\n"
          "            c[idx] = acc;\n"
          "        }\n    }\n}\n";
}
//=============================================================================
static ArrayOf
gpuScan(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& combineExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    // scanning a singleton dimension is the identity
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    std::string source = buildScanWgsl(combineExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, a->underlyingType());
    uint32_t params[3] = { lane.laneCount, lane.redLen, lane.before };
    uint32_t workgroupCountX = (lane.laneCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
gpuComplexScan(GpuArrayObject* a, int dim, const std::string& opKey,
    const std::string& combineReExpr, const std::string& combineImExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    std::string source
        = std::string("struct Params { laneCount : u32, redLen : u32, before : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let lane = gid.x;\n"
                      "    if (lane < params.laneCount) {\n"
                      "        let iB = lane % params.before;\n"
                      "        let iA = lane / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n"
                      "        var accRe : f32 = a[2u * start];\n"
                      "        var accIm : f32 = a[2u * start + 1u];\n"
                      "        c[2u * start] = accRe;\n"
                      "        c[2u * start + 1u] = accIm;\n"
                      "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
                      "            let idx = start + k * params.before;\n"
                      "            let vRe = a[2u * idx];\n"
                      "            let vIm = a[2u * idx + 1u];\n"
                      "            let nr = ")
        + combineReExpr + ";\n            let ni = " + combineImExpr
        + ";\n            accRe = nr;\n            accIm = ni;\n"
          "            c[2u * idx] = accRe;\n            c[2u * idx + 1u] = accIm;\n"
          "        }\n    }\n}\n";
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, "single", true);
    uint32_t params[3] = { lane.laneCount, lane.redLen, lane.before };
    uint32_t workgroupCountX = (lane.laneCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
// int32/uint32 prefix scan on the raw-bit storage: bitcast to i32/u32, combine
// with saturation at each step (cumsum/cumprod keep the integer class, unlike
// sum/prod), and write the running value back as bits.
static ArrayOf
gpuInt32Scan(GpuArrayObject* a, int dim, const std::string& op)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    std::string intType = a->underlyingType();
    bool isUnsigned = (intType == "uint32");
    const char* T = isUnsigned ? "u32" : "i32";

    std::string combine;
    if (op == "sum") {
        if (isUnsigned) {
            combine = "            let s = acc + v;\n"
                      "            if (s < acc) { acc = 4294967295u; } else { acc = s; }\n";
        } else {
            combine
                = "            let s = acc + v;\n"
                  "            if (acc > 0 && v > 0 && s < 0) { acc = 2147483647; }\n"
                  "            else if (acc < 0 && v < 0 && s >= 0) { acc = (-2147483647 - 1); }\n"
                  "            else { acc = s; }\n";
        }
    } else if (op == "max") {
        combine = "            if (v > acc) { acc = v; }\n";
    } else if (op == "min") {
        combine = "            if (v < acc) { acc = v; }\n";
    } else { // prod
        if (isUnsigned) {
            combine = "            let p = acc * v;\n"
                      "            if (acc != 0u && p / acc != v) { acc = 4294967295u; }\n"
                      "            else { acc = p; }\n";
        } else {
            combine = "            if (acc == (-2147483647 - 1) && v == -1) { acc = 2147483647; }\n"
                      "            else if (v == (-2147483647 - 1) && acc == -1) { acc = "
                      "2147483647; }\n"
                      "            else {\n"
                      "                let p = acc * v;\n"
                      "                var ov = false;\n"
                      "                if (acc != 0) { if (p / acc != v) { ov = true; } }\n"
                      "                if (ov) {\n"
                      "                    if ((acc > 0 && v > 0) || (acc < 0 && v < 0)) { acc = "
                      "2147483647; }\n"
                      "                    else { acc = (-2147483647 - 1); }\n"
                      "                } else { acc = p; }\n"
                      "            }\n";
        }
    }

    Dimensions dims = a->dimensions();
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity || lane.redLen <= 1 || dims.elementCount() == 0) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    std::string source
        = std::string("struct Params { laneCount : u32, redLen : u32, before : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let lane = gid.x;\n"
                      "    if (lane < params.laneCount) {\n"
                      "        let iB = lane % params.before;\n"
                      "        let iA = lane / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n"
                      "        var acc : ")
        + T + " = bitcast<" + T + ">(a[start]);\n" + "        c[start] = bitcast<f32>(acc);\n"
        + "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
        + "            let idx = start + k * params.before;\n" + "            let v = bitcast<" + T
        + ">(a[idx]);\n" + combine
        + "            c[idx] = bitcast<f32>(acc);\n        }\n    }\n}\n";

    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:i32scan:" + op + ":" + intType, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(dims, intType, false);
    uint32_t params[3] = { lane.laneCount, lane.redLen, lane.before };
    uint32_t workgroupCountX = (lane.laneCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuCumsum(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexScan(a, dim, "gpuArray:ccumsum", "accRe + vRe", "accIm + vIm");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "sum");
    }
    // int8/16 cumsum saturates at each step and keeps the class (values are
    // exact in f32, so the saturating combine reproduces the host result).
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return gpuScan(a, dim, "gpuArray:cumsum:" + intType, GpuSaturateExpr("acc + v", intType));
    }
    return gpuScan(a, dim, "gpuArray:cumsum", "acc + v");
}
//=============================================================================
ArrayOf
GpuCumprod(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexScan(
            a, dim, "gpuArray:ccumprod", "accRe * vRe - accIm * vIm", "accRe * vIm + accIm * vRe");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "prod");
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return gpuScan(a, dim, "gpuArray:cumprod:" + intType, GpuSaturateExpr("acc * v", intType));
    }
    return gpuScan(a, dim, "gpuArray:cumprod", "acc * v");
}
//=============================================================================
ArrayOf
GpuCummax(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "max");
    }
    std::string intType = GpuIntegerResultType(a, a);
    std::string key = intType.empty() ? "gpuArray:cummax" : "gpuArray:cummax:" + intType;
    return gpuScan(a, dim, key, "max(acc, v)");
}
//=============================================================================
ArrayOf
GpuCummin(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        return gpuInt32Scan(a, dim, "min");
    }
    std::string intType = GpuIntegerResultType(a, a);
    std::string key = intType.empty() ? "gpuArray:cummin" : "gpuArray:cummin:" + intType;
    return gpuScan(a, dim, key, "min(acc, v)");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
