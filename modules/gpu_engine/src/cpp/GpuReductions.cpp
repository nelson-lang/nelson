//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include <cstring>
#include <vector>
#include "GpuKernels.hpp"
#include "GpuElementwiseMath.hpp"
#include "GpuDispatch.hpp"
#include "GpuContext.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ReduceGeometry
{
    bool identity = false; // reducing a singleton dimension => copy input
    GpuLaneGeometry lane;
    Dimensions outputDims;
};
//=============================================================================
static ReduceGeometry
computeGeometry(const Dimensions& dims, int dim);
static void
checkUsable(GpuArrayObject* a);
// emptyValue fills the result of a zero-length reduction (0 for sum, 1 for prod, NaN for mean)
static ArrayOf
gpuReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& initExpr,
    const std::string& combineExpr, const std::string& finishExpr,
    const std::string& resultType = "single", float emptyValue = 0.0f);
static ArrayOfVector
gpuArgReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr);
//=============================================================================
std::string
GpuWgslIsNan(const std::string& value)
{
    return "((bitcast<u32>(" + value + ") & 0x7fffffffu) > 0x7f800000u)";
}
//=============================================================================
std::string
GpuWgslExtreme(const std::string& acc, const std::string& v, bool isMax, bool includeNan)
{
    std::string plain = std::string(isMax ? "max(" : "min(") + acc + ", " + v + ")";
    if (includeNan) {
        // the first NaN met is kept
        return "select(select(" + plain + ", " + v + ", " + GpuWgslIsNan(v) + "), " + acc + ", "
            + GpuWgslIsNan(acc) + ")";
    }
    return "select(select(" + plain + ", " + acc + ", " + GpuWgslIsNan(v) + "), " + v + ", "
        + GpuWgslIsNan(acc) + ")";
}
//=============================================================================
// argmax/argmin choice of v over acc, NaN handled like GpuWgslExtreme
static std::string
argCompare(bool isMax, bool includeNan)
{
    std::string better = isMax ? "v > acc" : "v < acc";
    std::string nv = GpuWgslIsNan("v");
    std::string na = GpuWgslIsNan("acc");
    if (includeNan) {
        return "(" + nv + " && !" + na + ") || (" + nv + " && " + na + " && kk < best) || (!" + nv
            + " && !" + na + " && " + better + ")";
    }
    return "!" + nv + " && (" + na + " || " + better + ")";
}
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
    return gpuReduce(a, dim, "gpuArray:sum", "v", "acc + v", "acc");
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
    return gpuReduce(a, dim, "gpuArray:prod", "v", "acc * v", "acc", "single", 1.0f);
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
    return gpuReduce(
        a, dim, "gpuArray:mean", "v", "acc + v", "acc / redLenF", "single", std::nanf(""));
}
//=============================================================================
ArrayOf
GpuMaxAlong(GpuArrayObject* a, int dim, bool includeNan)
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
            a, dim, "gpuArray:maxreduce:" + intType, "v", "max(acc, v)", "acc", intType);
    }
    return gpuReduce(a, dim, std::string("gpuArray:maxreduce:") + (includeNan ? "nan" : "omit"),
        "v", GpuWgslExtreme("acc", "v", true, includeNan), "acc");
}
//=============================================================================
ArrayOf
GpuMinAlong(GpuArrayObject* a, int dim, bool includeNan)
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
            a, dim, "gpuArray:minreduce:" + intType, "v", "min(acc, v)", "acc", intType);
    }
    return gpuReduce(a, dim, std::string("gpuArray:minreduce:") + (includeNan ? "nan" : "omit"),
        "v", GpuWgslExtreme("acc", "v", false, includeNan), "acc");
}
//=============================================================================
ArrayOf
GpuAny(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduceLogical(a, dim, "gpuArray:cany", "f32((acc != 0.0) || (nz != 0.0))");
    }
    // NaN is ignored by any
    std::string nonzero = "f32(v != 0.0 && !" + GpuWgslIsNan("v") + ")";
    return gpuReduce(a, dim, "gpuArray:any:nan", nonzero,
        "f32((acc != 0.0) || (" + nonzero + " != 0.0))", "acc", "logical");
}
//=============================================================================
ArrayOf
GpuAll(GpuArrayObject* a, int dim)
{
    if (a->isComplex()) {
        return gpuComplexReduceLogical(a, dim, "gpuArray:call", "f32((acc != 0.0) && (nz != 0.0))");
    }
    return gpuReduce(a, dim, "gpuArray:all", "f32(v != 0.0)", "f32((acc != 0.0) && (v != 0.0))",
        "acc", "logical", 1.0f);
}
//=============================================================================
ArrayOfVector
GpuMaxAlong2(GpuArrayObject* a, int dim, bool includeNan)
{
    return gpuArgReduce(a, dim, std::string("gpuArray:argmax:") + (includeNan ? "nan" : "omit"),
        argCompare(true, includeNan));
}
//=============================================================================
ArrayOfVector
GpuMinAlong2(GpuArrayObject* a, int dim, bool includeNan)
{
    return gpuArgReduce(a, dim, std::string("gpuArray:argmin:") + (includeNan ? "nan" : "omit"),
        argCompare(false, includeNan));
}
//=============================================================================
static ArrayOf
gpuVarStd(GpuArrayObject* a, int dim, bool normalizeByN, bool doSqrt)
{
    checkUsable(a);
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
    if (a->dimensions().elementCount() == 0 || geometry.lane.redLen == 0) {
        return ArrayOf::handle(output);
    }
    // running sum and sum of squares
    GpuLaneSpec spec;
    spec.state = { { "s", "f32" }, { "sq", "f32" } };
    spec.load = "    let v = a[e];\n    let v2 = v * v;\n";
    spec.mergeLoad = "    let v = other_s;\n    let v2 = other_sq;\n";
    spec.init = "    s = v;\n    sq = v2;\n";
    spec.update = "    s = s + v;\n    sq = sq + v2;\n";
    spec.store = std::string("    let numer = max(sq - s * s / redLenF, 0.0);\n"
                             "    var result : f32 = 0.0;\n"
                             "    if (params.redLen > 1u) {\n"
                             "        if (params.normN == 1u) { result = numer / redLenF; }\n"
                             "        else { result = numer / (redLenF - 1.0); }\n"
                             "    }\n"
                             "    c[o] = ")
        + (doSqrt ? "sqrt(result)" : "result") + ";\n";
    spec.outputs = { "c" };
    spec.extraParams = { "normN" };
    std::string opKey
        = std::string(doSqrt ? "gpuArray:std:" : "gpuArray:var:") + (normalizeByN ? "N" : "N1");
    try {
        GpuRunLaneReduce(opKey, spec, geometry.lane, a->buffer(), { output->buffer() },
            { normalizeByN ? 1u : 0u });
    } catch (...) {
        delete output;
        throw;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuVecnorm(GpuArrayObject* a, double p, int dim)
{
    checkUsable(a);
    if (a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        Error(L"Nelson:gpuArray:normRequiresFloat", L"norm requires a floating-point gpuArray.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return GpuAbs(a);
    }
    if (geometry.lane.redLen == 0) {
        return GpuFilled(geometry.outputDims, 0.0f, "single");
    }
    std::string magnitude = a->isComplex() ? "nlsHypot(a[2u * e], a[2u * e + 1u])" : "abs(a[e])";
    std::string nan = GpuWgslIsNan("vs");
    GpuLaneSpec spec;
    spec.outputs = { "c" };
    std::string key;
    if (p == 2.0) {
        // scale and sum of squares: the norm is scale * sqrt(ssq), with no overflow
        spec.state = { { "scale", "f32" }, { "ssq", "f32" } };
        spec.load = "    let vs = " + magnitude + ";\n    let vq = select(1.0, 0.0, vs == 0.0);\n";
        spec.mergeLoad = "    let vs = other_scale;\n    let vq = other_ssq;\n";
        spec.init = "    scale = vs;\n    ssq = vq;\n";
        spec.update = "    if (" + nan + " || " + GpuWgslIsNan("scale")
            + ") { scale = vs + scale; ssq = 1.0; }\n"
              "    else if (vs > 3.4e38 || scale > 3.4e38) { scale = max(vs, scale); ssq = 1.0; }\n"
              "    else if (vs > scale) { ssq = vq + ssq * (scale / vs) * (scale / vs); scale = "
              "vs; }\n"
              "    else if (vs > 0.0) { ssq = ssq + vq * (vs / scale) * (vs / scale); }\n";
        spec.store = "    c[o] = scale * sqrt(ssq);\n";
        key = "gpuArray:norm2";
    } else if (std::isinf(p)) {
        spec.state = { { "acc", "f32" } };
        spec.load = "    let vs = " + magnitude + ";\n";
        spec.mergeLoad = "    let vs = other_acc;\n";
        spec.init = "    acc = vs;\n";
        spec.update = "    acc = " + GpuWgslExtreme("acc", "vs", p > 0, true) + ";\n";
        spec.store = "    c[o] = acc;\n";
        key = p > 0 ? "gpuArray:norminf" : "gpuArray:normneginf";
    } else {
        // sum of |x|^p, then the p-th root (p == 1 is the plain sum)
        spec.state = { { "acc", "f32" } };
        spec.load = "    let vs = pow(" + magnitude + ", bitcast<f32>(params.p));\n";
        spec.mergeLoad = "    let vs = other_acc;\n";
        spec.init = "    acc = vs;\n";
        spec.update = "    acc = acc + vs;\n";
        spec.store = "    c[o] = pow(acc, 1.0 / bitcast<f32>(params.p));\n";
        if (p == 1.0) {
            spec.load = "    let vs = " + magnitude + ";\n";
            spec.store = "    c[o] = acc;\n";
        }
        spec.extraParams = { "p" };
        key = p == 1.0 ? "gpuArray:norm1" : "gpuArray:normp";
    }
    // the accurate library supplies nlsHypot for complex magnitudes
    if (a->isComplex()) {
        spec.helpers = GpuMathHelpersWgsl();
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", false);
    float pf = (float)p;
    uint32_t pBits;
    std::memcpy(&pBits, &pf, sizeof(pBits));
    try {
        GpuRunLaneReduce(key + (a->isComplex() ? ":c" : ""), spec, geometry.lane, a->buffer(),
            { output->buffer() },
            spec.extraParams.empty() ? std::vector<uint32_t> {} : std::vector<uint32_t> { pBits });
    } catch (...) {
        delete output;
        throw;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// middle of every sorted lane; NaN sit at the lane end after sorting
static const char* MEDIAN_PICK_WGSL = R"WGSL(
struct Params { laneCount : u32, redLen : u32, before : u32, omitNan : u32 };
@group(0) @binding(0) var<storage, read> s : array<f32>;
@group(0) @binding(1) var<storage, read_write> c : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
fn isNan(t : f32) -> bool { return (bitcast<u32>(t) & 0x7fffffffu) > 0x7f800000u; }
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let o = gid.x;
    if (o >= params.laneCount) { return; }
    let start = o % params.before + (o / params.before) * params.before * params.redLen;
    let step = params.before;
    var nanBits : u32 = 0x7fc00000u;
    var count = params.redLen;
    if (isNan(s[start + (count - 1u) * step])) {
        if (params.omitNan == 0u) { c[o] = bitcast<f32>(nanBits); return; }
        // first NaN position by bisection
        var lo = 0u;
        var hi = count - 1u;
        loop {
            if (lo >= hi) { break; }
            let mid = (lo + hi) / 2u;
            if (isNan(s[start + mid * step])) { hi = mid; } else { lo = mid + 1u; }
        }
        count = lo;
    }
    if (count == 0u) { c[o] = bitcast<f32>(nanBits); return; }
    let a = s[start + ((count - 1u) / 2u) * step];
    let b = s[start + (count / 2u) * step];
    // written to avoid overflow for large values of the same sign
    c[o] = select(a + (b - a) * 0.5, a, a == b);
}
)WGSL";
//=============================================================================
ArrayOf
GpuMedian(GpuArrayObject* a, int dim, bool omitNan)
{
    checkUsable(a);
    if (a->isComplex() || a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        Error(L"Nelson:gpuArray:medianRequiresFloat",
            L"median requires a real floating-point gpuArray.");
    }
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    if (geometry.lane.redLen == 0) {
        return GpuFilled(geometry.outputDims, std::nanf(""), "single");
    }
    ArrayOfVector sorted = GpuSort(a, geometry.lane.dimension, false);
    GpuArrayObject* s = AsGpuArrayObject(sorted[0]);
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", false);
    ArrayOf result = ArrayOf::handle(output);
    WGPUComputePipeline pipeline
        = GpuContext::getInstance()->pipelineFor("gpuArray:medianpick", MEDIAN_PICK_WGSL, "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    uint32_t params[4] = { geometry.lane.laneCount, geometry.lane.redLen, geometry.lane.before,
        omitNan ? 1u : 0u };
    GpuDispatchKernel(pipeline, { s->buffer(), output->buffer() }, params, 4,
        (geometry.lane.laneCount + 255u) / 256u, 1);
    return result;
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
    checkUsable(a);
    GpuContext* context = GpuContext::getInstance();
    std::string intType = a->underlyingType();
    bool isUnsigned = (intType == "uint32");
    const std::string T = isUnsigned ? "u32" : "i32";

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

    std::string resultType = floatMode ? "single" : intType;
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, resultType, false);
    if (a->dimensions().elementCount() == 0 || geometry.lane.redLen == 0) {
        return ArrayOf::handle(output);
    }
    GpuLaneSpec spec;
    spec.mergeLoad = "    let v = other_acc;\n";
    spec.init = "    acc = v;\n";
    if (floatMode) {
        // accumulate in f32 (bitcast raw int -> integer -> float), output single
        spec.state = { { "acc", "f32" } };
        spec.load = "    let v = f32(bitcast<" + T + ">(a[e]));\n";
        spec.update = (op == "prod") ? "    acc = acc * v;\n" : "    acc = acc + v;\n";
        spec.store = (op == "mean") ? "    c[o] = acc / redLenF;\n" : "    c[o] = acc;\n";
    } else {
        spec.state = { { "acc", T } };
        spec.load = "    let v = bitcast<" + T + ">(a[e]);\n";
        spec.update = (op == "max") ? "    acc = max(acc, v);\n" : "    acc = min(acc, v);\n";
        spec.store = "    c[o] = bitcast<f32>(acc);\n";
    }
    spec.outputs = { "c" };
    try {
        GpuRunLaneReduce("gpuArray:i32reduce:" + op + ":" + intType, spec, geometry.lane,
            a->buffer(), { output->buffer() });
    } catch (...) {
        delete output;
        throw;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
static void
checkUsable(GpuArrayObject* a)
{
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!GpuContext::getInstance()->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
}
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
    geometry.lane = lane;

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
// run `spec` into a new output array, freeing the outputs if the dispatch fails
static void
runReduce(const std::string& opKey, const GpuLaneSpec& spec, const ReduceGeometry& geometry,
    GpuArrayObject* a, const std::vector<GpuArrayObject*>& outputs)
{
    std::vector<WGPUBuffer> buffers;
    for (GpuArrayObject* output : outputs) {
        buffers.push_back(output->buffer());
    }
    try {
        GpuRunLaneReduce(opKey, spec, geometry.lane, a->buffer(), buffers);
    } catch (...) {
        for (GpuArrayObject* output : outputs) {
            delete output;
        }
        throw;
    }
}
//=============================================================================
static ArrayOf
gpuReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& initExpr,
    const std::string& combineExpr, const std::string& finishExpr, const std::string& resultType,
    float emptyValue)
{
    checkUsable(a);
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
            initExpr == "v" ? "ai" : "f32(ai != 0.0)", resultType);
    }
    if (geometry.lane.redLen == 0) {
        return GpuFilled(geometry.outputDims, emptyValue, resultType);
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, resultType);
    if (a->dimensions().elementCount() == 0) {
        return ArrayOf::handle(output);
    }
    GpuLaneSpec spec;
    spec.state = { { "acc", "f32" } };
    spec.load = "    let v = a[e];\n";
    spec.mergeLoad = "    let v = other_acc;\n";
    spec.init = "    acc = " + initExpr + ";\n";
    spec.update = "    acc = " + combineExpr + ";\n";
    spec.store = "    c[o] = " + finishExpr + ";\n";
    spec.outputs = { "c" };
    runReduce(opKey, spec, geometry, a, { output });
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOfVector
gpuArgReduce(GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr)
{
    checkUsable(a);
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
    GpuArrayObject* values = AllocateGpuArray(geometry.outputDims);
    GpuArrayObject* indices = AllocateGpuArray(geometry.outputDims);
    if (a->dimensions().elementCount() > 0 && geometry.lane.redLen > 0) {
        // on a tie the lower position wins, as in a left-to-right scan
        GpuLaneSpec spec;
        spec.state = { { "acc", "f32" }, { "best", "u32" } };
        spec.load = "    let v = a[e];\n    let kk = k;\n";
        spec.mergeLoad = "    let v = other_acc;\n    let kk = other_best;\n";
        spec.init = "    acc = v;\n    best = kk;\n";
        spec.update = "    if ((" + compareExpr
            + ") || (v == acc && kk < best)) {\n        acc = v;\n        best = kk;\n    }\n";
        spec.store = "    vals[o] = acc;\n    idx[o] = f32(best + 1u);\n";
        spec.outputs = { "vals", "idx" };
        runReduce(opKey, spec, geometry, a, { values, indices });
    }
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
    checkUsable(a);
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", true);
    if (a->dimensions().elementCount() == 0 || geometry.lane.redLen == 0) {
        return ArrayOf::handle(output);
    }
    // interleaved (re, im) values
    GpuLaneSpec spec;
    spec.state = { { "accRe", "f32" }, { "accIm", "f32" } };
    spec.load = "    let vRe = a[2u * e];\n    let vIm = a[2u * e + 1u];\n";
    spec.mergeLoad = "    let vRe = other_accRe;\n    let vIm = other_accIm;\n";
    spec.init = "    accRe = vRe;\n    accIm = vIm;\n";
    spec.update = "    let nr = " + combineReExpr + ";\n    let ni = " + combineImExpr
        + ";\n    accRe = nr;\n    accIm = ni;\n";
    spec.store
        = "    c[2u * o] = " + finishReExpr + ";\n    c[2u * o + 1u] = " + finishImExpr + ";\n";
    spec.outputs = { "c" };
    runReduce(opKey, spec, geometry, a, { output });
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
gpuComplexReduceLogical(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& combineExpr)
{
    checkUsable(a);
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        // reducing along a trailing singleton dimension: rarely requested for
        // any/all; not handled for complex yet.
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "logical", false);
    if (a->dimensions().elementCount() == 0 || geometry.lane.redLen == 0) {
        return ArrayOf::handle(output);
    }
    // nonzero test on interleaved (re, im); reduce booleans to a logical result
    GpuLaneSpec spec;
    spec.state = { { "acc", "f32" } };
    spec.load = "    let nz = f32((a[2u * e] != 0.0) || (a[2u * e + 1u] != 0.0));\n";
    spec.mergeLoad = "    let nz = other_acc;\n";
    spec.init = "    acc = nz;\n";
    spec.update = "    acc = " + combineExpr + ";\n";
    spec.store = "    c[o] = acc;\n";
    spec.outputs = { "c" };
    runReduce(opKey, spec, geometry, a, { output });
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
gpuComplexMaxMin(
    GpuArrayObject* a, int dim, const std::string& opKey, const std::string& compareExpr)
{
    checkUsable(a);
    ReduceGeometry geometry = computeGeometry(a->dimensions(), dim);
    if (geometry.identity) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    GpuArrayObject* output = AllocateGpuArray(geometry.outputDims, "single", true);
    if (a->dimensions().elementCount() == 0 || geometry.lane.redLen == 0) {
        return ArrayOf::handle(output);
    }
    // keep the element with the extreme magnitude (first one on a tie)
    GpuLaneSpec spec;
    spec.state
        = { { "bestRe", "f32" }, { "bestIm", "f32" }, { "bestMag", "f32" }, { "bestK", "u32" } };
    spec.load = "    let vRe = a[2u * e];\n    let vIm = a[2u * e + 1u];\n"
                "    let vMag = vRe * vRe + vIm * vIm;\n    let kk = k;\n";
    spec.mergeLoad = "    let vRe = other_bestRe;\n    let vIm = other_bestIm;\n"
                     "    let vMag = other_bestMag;\n    let kk = other_bestK;\n";
    spec.init = "    bestRe = vRe;\n    bestIm = vIm;\n    bestMag = vMag;\n    bestK = kk;\n";
    spec.update = "    if ((" + compareExpr
        + ") || (vMag == bestMag && kk < bestK)) {\n"
          "        bestRe = vRe;\n        bestIm = vIm;\n        bestMag = vMag;\n"
          "        bestK = kk;\n    }\n";
    spec.store = "    c[2u * o] = bestRe;\n    c[2u * o + 1u] = bestIm;\n";
    spec.outputs = { "c" };
    runReduce(opKey, spec, geometry, a, { output });
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
