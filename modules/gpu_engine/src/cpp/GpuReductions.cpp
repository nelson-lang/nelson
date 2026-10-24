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
// Reduction geometry for a column-major array reduced along `dim` (0-based).
struct ReduceGeometry
{
    bool identity = false; // reducing a singleton dimension => copy input
    uint32_t outCount = 0;
    uint32_t redLen = 0;
    uint32_t before = 0;
    Dimensions outputDims;
};
//=============================================================================
static ReduceGeometry
computeGeometry(const Dimensions& dims, int dim);
static ArrayOf
gpuReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& initExpr,
    const std::string& combineExpr, const std::string& finishExpr,
    const std::string& resultType = "single");
static ArrayOfVector
gpuArgReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr);
static ArrayOf
gpuComplexReduce(GpuArrayObject* a, int dim, const std::string& opKey,
    const std::string& combineReExpr, const std::string& combineImExpr,
    const std::string& finishReExpr, const std::string& finishImExpr);
static ArrayOf
gpuComplexReduceLogical(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& combineExpr);
static ArrayOf
gpuComplexMaxMin(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr);
//=============================================================================
ArrayOf
GpuSum(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduce(
            a, dim, "gpuArray:csum", "accRe + vRe", "accIm + vIm", "accRe", "accIm");
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Reduce(a, dim, "sum");
    }
    // integer sum() promotes to a floating result (no saturation), matching the
    // host default; int8/16 already store f32 values so the plain path applies.
    return gpuReduce(a, dim, "gpuArray:sum", "a[start]", "acc + v", "acc");
}
//=============================================================================
ArrayOf
GpuProd(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduce(a, dim, "gpuArray:cprod", "accRe * vRe - accIm * vIm",
            "accRe * vIm + accIm * vRe", "accRe", "accIm");
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Reduce(a, dim, "prod");
    }
    // integer prod() promotes to a floating result (no saturation), like sum().
    return gpuReduce(a, dim, "gpuArray:prod", "a[start]", "acc * v", "acc");
}
//=============================================================================
ArrayOf
GpuMean(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduce(a, dim, "gpuArray:cmean", "accRe + vRe", "accIm + vIm",
            "accRe / redLenF", "accIm / redLenF");
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Reduce(a, dim, "mean");
    }
    return gpuReduce(a, dim, "gpuArray:mean", "a[start]", "acc + v", "acc / redLenF");
}
//=============================================================================
ArrayOf
GpuMaxAlong(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        // complex ordering is by magnitude
        return gpuComplexMaxMin(a, dim, "gpuArray:cmax", "vMag > bestMag");
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Reduce(a, dim, "max");
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        // extreme value is already in range: keep the class tag, no saturation
        return gpuReduce(
            a, dim, "gpuArray:maxreduce:" + intType, "a[start]", "max(acc, v)", "acc", intType);
    }
    return gpuReduce(a, dim, "gpuArray:maxreduce", "a[start]", "max(acc, v)", "acc");
}
//=============================================================================
ArrayOf
GpuMinAlong(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexMaxMin(a, dim, "gpuArray:cmin", "vMag < bestMag");
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Reduce(a, dim, "min");
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return gpuReduce(
            a, dim, "gpuArray:minreduce:" + intType, "a[start]", "min(acc, v)", "acc", intType);
    }
    return gpuReduce(a, dim, "gpuArray:minreduce", "a[start]", "min(acc, v)", "acc");
}
//=============================================================================
ArrayOf
GpuAny(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduceLogical(a, dim, "gpuArray:cany", "f32((acc != 0.0) || (nz != 0.0))");
    }
    return gpuReduce(a, dim, "gpuArray:any", "f32(a[start] != 0.0)",
        "f32((acc != 0.0) || (v != 0.0))", "acc", "logical");
}
//=============================================================================
ArrayOf
GpuAll(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduceLogical(a, dim, "gpuArray:call", "f32((acc != 0.0) && (nz != 0.0))");
    }
    return gpuReduce(a, dim, "gpuArray:all", "f32(a[start] != 0.0)",
        "f32((acc != 0.0) && (v != 0.0))", "acc", "logical");
}
//=============================================================================
ArrayOfVector
GpuMaxAlong2(GpuArrayObject* a, int dim)
{
    return gpuArgReduce(a, dim, "gpuArray:argmax", "v > acc");
}
//=============================================================================
ArrayOfVector
GpuMinAlong2(GpuArrayObject* a, int dim)
{
    return gpuArgReduce(a, dim, "gpuArray:argmin", "v < acc");
}
//=============================================================================
static ArrayOf
gpuVarStd(GpuArrayObject* a, int dim, bool normalizeByN, bool doSqrt)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (a->isComplex() || a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        Error(
            L"Nelson:gpuArray:varStdRequiresFloat", L"var/std require a floating-point gpuArray.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        // variance of a single element is 0
        Dimensions dims = a->dimensions();
        indexType n = dims.elementCount();
        single* zeros = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, n);
        for (indexType k = 0; k < n; ++k) {
            zeros[k] = 0.0f;
        }
        return CreateGpuArray(ArrayOf(NLS_SINGLE, dims, zeros));
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single");
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        return ArrayOf::handle(output);
    }
    std::string finish = doSqrt ? "sqrt(result)" : "result";
    std::string source
        = std::string("struct Params { outCount : u32, redLen : u32, before : u32, normN : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let o = gid.x;\n"
                      "    if (o < params.outCount) {\n"
                      "        let iB = o % params.before;\n"
                      "        let iA = o / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n"
                      "        var s : f32 = 0.0;\n"
                      "        var sq : f32 = 0.0;\n"
                      "        for (var k : u32 = 0u; k < params.redLen; k = k + 1u) {\n"
                      "            let v = a[start + k * params.before];\n"
                      "            s = s + v;\n"
                      "            sq = sq + v * v;\n"
                      "        }\n"
                      "        let n = f32(params.redLen);\n"
                      "        let numer = max(sq - s * s / n, 0.0);\n"
                      "        var result : f32 = 0.0;\n"
                      "        if (params.redLen > 1u) {\n"
                      "            if (params.normN == 1u) { result = numer / n; }\n"
                      "            else { result = numer / (n - 1.0); }\n"
                      "        }\n"
                      "        c[o] = ")
        + finish + ";\n    }\n}\n";
    std::string opKey
        = std::string(doSqrt ? "gpuArray:std:" : "gpuArray:var:") + (normalizeByN ? "N" : "N1");
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[4]
        = { geometry.outCount, geometry.redLen, geometry.before, normalizeByN ? 1u : 0u };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 4, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuVar(GpuArrayObject* a, int dim, bool normalizeByN)
{
    return gpuVarStd(a, dim, normalizeByN, false);
}
//=============================================================================
ArrayOf
GpuStd(GpuArrayObject* a, int dim, bool normalizeByN)
{
    return gpuVarStd(a, dim, normalizeByN, true);
}
//=============================================================================
ArrayOf
GpuInt32Reduce(GpuArrayObject* a, int dim, const std::string& op)
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

    // sum/prod/mean promote to a floating result (host default: no saturation);
    // max/min keep the integer class. floatMode selects f32 accumulation.
    bool floatMode = (op == "sum" || op == "prod" || op == "mean");
    if (!floatMode && op != "max" && op != "min") {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }

    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        if (floatMode) {
            // reducing a singleton dimension: copy the values into a single array
            std::string copySource
                = std::string(
                      "struct Params { n : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n    if (i < params.n) {\n        c[i] = f32(bitcast<")
                + T + ">(a[i]));\n    }\n}\n";
            WGPUComputePipeline copyPipeline = context->pipelineFor(
                std::string("gpuArray:i32tofloat:") + intType, copySource.c_str(), "main");
            if (copyPipeline == nullptr) {
                Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
            }
            Dimensions dims = a->dimensions();
            GpuArrayObject* copyOut = AllocateGpuArray(dims, "single", false);
            uint32_t n = (uint32_t)dims.elementCount();
            if (n > 0) {
                uint32_t p[1] = { n };
                GpuDispatchKernel(
                    copyPipeline, { a->buffer(), copyOut->buffer() }, p, 1, (n + 255u) / 256u, 1);
            }
            return ArrayOf::handle(copyOut);
        }
        // max/min of a singleton dimension is a copy (values already in range)
        return ArrayOf::handle(CloneGpuArray(a));
    }

    // per-op combine over the loaded value `v` updating the accumulator `acc`
    std::string combine;
    std::string finish = "acc";
    if (floatMode) {
        combine = (op == "prod") ? "            acc = acc * v;\n" : "            acc = acc + v;\n";
        if (op == "mean") {
            finish = "acc / f32(params.redLen)";
        }
    } else if (op == "max") {
        combine = "            acc = max(acc, v);\n";
    } else {
        combine = "            acc = min(acc, v);\n";
    }

    std::string resultType = floatMode ? "single" : intType;
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, resultType, false);
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        return ArrayOf::handle(output);
    }

    std::string loadInit;
    std::string loadStep;
    std::string store;
    if (floatMode) {
        // accumulate in f32 (bitcast raw int -> integer -> float), output single
        loadInit = std::string("        var acc : f32 = f32(bitcast<") + T + ">(a[start]));\n";
        loadStep = std::string("            let v = f32(bitcast<") + T
            + ">(a[start + k * params.before]));\n";
        store = std::string("        c[o] = ") + finish + ";\n";
    } else {
        loadInit = std::string("        var acc : ") + T + " = bitcast<" + T + ">(a[start]);\n";
        loadStep = std::string("            let v = bitcast<") + T
            + ">(a[start + k * params.before]);\n";
        store = "        c[o] = bitcast<f32>(acc);\n";
    }

    std::string source
        = std::string("struct Params { outCount : u32, redLen : u32, before : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let o = gid.x;\n"
                      "    if (o < params.outCount) {\n"
                      "        let iB = o % params.before;\n"
                      "        let iA = o / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n")
        + loadInit + "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n" + loadStep
        + combine + "        }\n" + store + "    }\n}\n";

    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:i32reduce:" + op + ":" + intType, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
// static helper functions
//=============================================================================
static ReduceGeometry
computeGeometry(const Dimensions& dims, int dim)
{
    ReduceGeometry geometry;
    GpuLaneGeometry lane = GpuComputeLaneGeometry(dims, dim);
    if (lane.identity) {
        geometry.identity = true;
        return geometry;
    }
    geometry.before = lane.before;
    geometry.redLen = lane.redLen;
    geometry.outCount = lane.laneCount;

    indexType ndims = dims.length();
    if (ndims < 2) {
        ndims = 2;
    }
    std::vector<indexType> outVector((size_t)ndims);
    for (indexType k = 0; k < ndims; ++k) {
        outVector[(size_t)k] = dims.dimensionLength(k);
    }
    outVector[(size_t)(lane.dimension - 1)] = 1;
    geometry.outputDims = Dimensions(outVector);
    // drop trailing singleton dimensions, e.g. sum(3x4x5, 3) -> 3x4 (not 3x4x1)
    geometry.outputDims.simplify();
    return geometry;
}
//=============================================================================
static ArrayOf
gpuReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& initExpr,
    const std::string& combineExpr, const std::string& finishExpr, const std::string& resultType)
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
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        // preserve the requested result type (e.g. any/all of a singleton dim)
        return GpuUnaryElementwise(a, "gpuArray:copyto:" + resultType,
            initExpr == "a[start]" ? "ai" : "f32(ai != 0.0)", resultType);
    }
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        GpuArrayObject* emptyOutput = AllocateGpuArray(geometry.outputDims, resultType);
        return ArrayOf::handle(emptyOutput);
    }
    std::string source = GpuBuildReduceWgsl(initExpr, combineExpr, finishExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, resultType);
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOfVector
gpuArgReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (a->isComplex() || a->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:typeUnsupported",
            L"This operation is not supported for this gpuArray type yet.");
    }
    ArrayOfVector retval;
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        // reducing a singleton dimension: values = copy, indices all 1
        retval << GpuUnaryElementwise(a, "gpuArray:copy", "ai");
        retval << GpuUnaryElementwise(a, "gpuArray:oneidx", "1.0");
        return retval;
    }
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        retval << ArrayOf::handle(AllocateGpuArray(geometry.outputDims));
        retval << ArrayOf::handle(AllocateGpuArray(geometry.outputDims));
        return retval;
    }
    std::string source = GpuBuildArgReduceWgsl(compareExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* values = AllocateGpuArray(geometry.outputDims);
    GpuArrayObject* indices = AllocateGpuArray(geometry.outputDims);
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), values->buffer(), indices->buffer() }, params, 3,
        workgroupCountX, 1);
    retval << ArrayOf::handle(values);
    retval << ArrayOf::handle(indices);
    return retval;
}
//=============================================================================
static ArrayOf
gpuComplexReduce(GpuArrayObject* a, int dim, const std::string& opKey,
    const std::string& combineReExpr, const std::string& combineImExpr,
    const std::string& finishReExpr, const std::string& finishImExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        return ArrayOf::handle(AllocateGpuArray(geometry.outputDims, "single", true));
    }
    // interleaved (re, im) reduction: one thread per output element
    std::string source
        = std::string("struct Params { outCount : u32, redLen : u32, before : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let o = gid.x;\n"
                      "    if (o < params.outCount) {\n"
                      "        let iB = o % params.before;\n"
                      "        let iA = o / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n"
                      "        var accRe : f32 = a[2u * start];\n"
                      "        var accIm : f32 = a[2u * start + 1u];\n"
                      "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
                      "            let idx = start + k * params.before;\n"
                      "            let vRe = a[2u * idx];\n"
                      "            let vIm = a[2u * idx + 1u];\n"
                      "            let nr = ")
        + combineReExpr + ";\n            let ni = " + combineImExpr
        + ";\n            accRe = nr;\n            accIm = ni;\n        }\n"
          "        let redLenF = f32(params.redLen);\n"
          "        c[2u * o] = "
        + finishReExpr + ";\n        c[2u * o + 1u] = " + finishImExpr + ";\n    }\n}\n";

    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", true);
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
gpuComplexReduceLogical(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& combineExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        // reducing along a trailing singleton dimension: rarely requested for
        // any/all; not handled for complex yet.
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        return ArrayOf::handle(AllocateGpuArray(geometry.outputDims, "logical", false));
    }
    // nonzero test on interleaved (re, im); reduce booleans to a logical result
    std::string source
        = std::string(
              "struct Params { outCount : u32, redLen : u32, before : u32 };\n"
              "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
              "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
              "@group(0) @binding(2) var<uniform> params : Params;\n"
              "@compute @workgroup_size(256)\n"
              "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
              "    let o = gid.x;\n"
              "    if (o < params.outCount) {\n"
              "        let iB = o % params.before;\n"
              "        let iA = o / params.before;\n"
              "        let start = iB + iA * (params.before * params.redLen);\n"
              "        var acc : f32 = f32((a[2u * start] != 0.0) || (a[2u * start + 1u] != "
              "0.0));\n"
              "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
              "            let idx = start + k * params.before;\n"
              "            let nz = f32((a[2u * idx] != 0.0) || (a[2u * idx + 1u] != 0.0));\n"
              "            acc = ")
        + combineExpr + ";\n        }\n        c[o] = acc;\n    }\n}\n";
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "logical", false);
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
gpuComplexMaxMin(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    if (a->dimensions().elementCount() == 0 || geometry.redLen == 0) {
        return ArrayOf::handle(AllocateGpuArray(geometry.outputDims, "single", true));
    }
    // keep the element with the extreme magnitude; output the complex value
    std::string source
        = std::string("struct Params { outCount : u32, redLen : u32, before : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let o = gid.x;\n"
                      "    if (o < params.outCount) {\n"
                      "        let iB = o % params.before;\n"
                      "        let iA = o / params.before;\n"
                      "        let start = iB + iA * (params.before * params.redLen);\n"
                      "        var bestRe : f32 = a[2u * start];\n"
                      "        var bestIm : f32 = a[2u * start + 1u];\n"
                      "        var bestMag : f32 = bestRe * bestRe + bestIm * bestIm;\n"
                      "        for (var k : u32 = 1u; k < params.redLen; k = k + 1u) {\n"
                      "            let idx = start + k * params.before;\n"
                      "            let vRe = a[2u * idx];\n"
                      "            let vIm = a[2u * idx + 1u];\n"
                      "            let vMag = vRe * vRe + vIm * vIm;\n"
                      "            if (")
        + compareExpr
        + ") {\n"
          "                bestRe = vRe;\n                bestIm = vIm;\n                bestMag = "
          "vMag;\n"
          "            }\n        }\n"
          "        c[2u * o] = bestRe;\n        c[2u * o + 1u] = bestIm;\n    }\n}\n";
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", true);
    uint32_t params[3] = { geometry.outCount, geometry.redLen, geometry.before };
    uint32_t workgroupCountX = (geometry.outCount + 255u) / 256u;
    GpuDispatchKernel(pipeline, { a->buffer(), output->buffer() }, params, 3, workgroupCountX, 1);
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
