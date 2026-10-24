//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// int32 / uint32 gpuArray kernels.
//
// The device storage buffer is always array<f32>; for 32-bit integers it holds
// the raw integer bits (a 32-bit int and an f32 are both 4 bytes), and every
// kernel bitcasts them back to i32/u32 so the arithmetic is exact. Overflow is
// detected with pure integer arithmetic (no lossy f32 magnitude test) and the
// result saturates to the class range, matching host integer semantics.
//=============================================================================
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
// WGSL cannot spell the literals 2147483648 / -2147483648 directly (the token
// 2147483648 overflows i32), so INT32_MIN is written as (-2147483647 - 1).
#define WGSL_I32_MAX "2147483647"
#define WGSL_I32_MIN "(-2147483647 - 1)"
#define WGSL_U32_MAX "4294967295u"
//=============================================================================
static GpuArrayObject*
materializeInt(GpuArrayObject* a, const std::string& intType, bool& owned);
static ArrayOf
runInt32Binary(GpuArrayObject* a, GpuArrayObject* b, const std::string& intType,
    const std::string& opKey, const std::string& body);
//=============================================================================
std::string
GpuInt32ResultType(GpuArrayObject* a, GpuArrayObject* b)
{
    GpuStorage sa = a->storageKind();
    GpuStorage sb = b->storageKind();
    bool ra = (sa != GpuStorage::Float32);
    bool rb = (sb != GpuStorage::Float32);
    if (!ra && !rb) {
        return "";
    }
    if (ra && rb) {
        if (sa != sb) {
            Error(L"Nelson:gpuArray:integerTypeMismatch", L"Integer types must match.");
        }
        return a->underlyingType();
    }
    // one 32-bit integer, one f32-storage operand: reject small integer classes
    // (int8/16 must match), allow a real (single/logical) operand.
    GpuArrayObject* rawOne = ra ? a : b;
    GpuArrayObject* other = ra ? b : a;
    NelsonType hostClass = NLS_SINGLE;
    std::string lo, hi;
    if (GpuIntegerUnderlyingType(other->underlyingType(), hostClass, lo, hi)) {
        Error(L"Nelson:gpuArray:integerTypeMismatch", L"Integer types must match.");
    }
    return rawOne->underlyingType();
}
//=============================================================================
ArrayOf
GpuInt32Binary(GpuArrayObject* a, GpuArrayObject* b, const std::string& op)
{
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    std::string intType = GpuInt32ResultType(a, b);
    bool isUnsigned = (intType == "uint32");
    const char* T = isUnsigned ? "u32" : "i32";
    const char* MAXV = isUnsigned ? WGSL_U32_MAX : WGSL_I32_MAX;
    const char* MINV = isUnsigned ? "0u" : WGSL_I32_MIN;

    std::string body;
    if (op == "plus") {
        if (isUnsigned) {
            body = "        let s = ai + bi;\n"
                   "        if (s < ai) { res = " WGSL_U32_MAX "; } else { res = s; }\n";
        } else {
            body = "        let s = ai + bi;\n"
                   "        if (ai > 0 && bi > 0 && s < 0) { res = " WGSL_I32_MAX "; }\n"
                   "        else if (ai < 0 && bi < 0 && s >= 0) { res = " WGSL_I32_MIN "; }\n"
                   "        else { res = s; }\n";
        }
    } else if (op == "minus") {
        if (isUnsigned) {
            body = "        if (bi > ai) { res = 0u; } else { res = ai - bi; }\n";
        } else {
            body = "        let s = ai - bi;\n"
                   "        if (ai >= 0 && bi < 0 && s < 0) { res = " WGSL_I32_MAX "; }\n"
                   "        else if (ai < 0 && bi > 0 && s >= 0) { res = " WGSL_I32_MIN "; }\n"
                   "        else { res = s; }\n";
        }
    } else if (op == "times") {
        if (isUnsigned) {
            body = "        let p = ai * bi;\n"
                   "        if (ai != 0u && p / ai != bi) { res = " WGSL_U32_MAX "; }\n"
                   "        else { res = p; }\n";
        } else {
            body = "        if (ai == " WGSL_I32_MIN " && bi == -1) { res = " WGSL_I32_MAX "; }\n"
                   "        else if (bi == " WGSL_I32_MIN " && ai == -1) { res = " WGSL_I32_MAX
                   "; }\n"
                   "        else {\n"
                   "            let p = ai * bi;\n"
                   "            var ov = false;\n"
                   "            if (ai != 0) { if (p / ai != bi) { ov = true; } }\n"
                   "            if (ov) {\n"
                   "                if ((ai > 0 && bi > 0) || (ai < 0 && bi < 0)) { res "
                   "= " WGSL_I32_MAX "; }\n"
                   "                else { res = " WGSL_I32_MIN "; }\n"
                   "            } else { res = p; }\n"
                   "        }\n";
        }
    } else if (op == "div") {
        if (isUnsigned) {
            // round half away from zero; x/0 -> max (0/0 -> 0). r >= bi - r avoids
            // the overflow of 2*r for large operands.
            body = "        if (bi == 0u) {\n"
                   "            if (ai == 0u) { res = 0u; } else { res = " WGSL_U32_MAX "; }\n"
                   "        } else {\n"
                   "            let q = ai / bi;\n"
                   "            let r = ai - q * bi;\n"
                   "            if (r >= bi - r) { res = q + 1u; } else { res = q; }\n"
                   "        }\n";
        } else {
            // round half away from zero; x/0 -> intmax*sign(x); INT32_MIN/-1 -> max.
            // Magnitudes are taken in u32 (|INT32_MIN| = 2^31 fits) and the
            // quotient sign is sign(ai) xor sign(bi) via the sign bit of ai ^ bi.
            body = "        if (bi == 0) {\n"
                   "            if (ai > 0) { res = " WGSL_I32_MAX "; }\n"
                   "            else if (ai < 0) { res = " WGSL_I32_MIN "; } else { res = 0; }\n"
                   "        } else if (ai == " WGSL_I32_MIN " && bi == -1) { res = " WGSL_I32_MAX
                   "; }\n"
                   "        else {\n"
                   "            let q = ai / bi;\n"
                   "            let r = ai - q * bi;\n"
                   "            var ur : u32 = bitcast<u32>(r); if (r < 0) { ur = 0u - ur; }\n"
                   "            var ub : u32 = bitcast<u32>(bi); if (bi < 0) { ub = 0u - ub; }\n"
                   "            if (ur >= ub - ur) {\n"
                   "                if ((ai ^ bi) < 0) { res = q - 1; } else { res = q + 1; }\n"
                   "            } else { res = q; }\n"
                   "        }\n";
        }
    } else if (op == "max") {
        body = "        res = max(ai, bi);\n";
    } else if (op == "min") {
        body = "        res = min(ai, bi);\n";
    } else if (op == "bitand") {
        body = "        res = ai & bi;\n";
    } else if (op == "bitor") {
        body = "        res = ai | bi;\n";
    } else if (op == "bitxor") {
        body = "        res = ai ^ bi;\n";
    } else if (op == "bitshift") {
        // bi holds the (signed) shift amount; a u32 operand round-trips the sign
        // through bitcast. Positive shifts left, negative shifts right; a shift
        // of 32 or more truncates to 0 (matches the host truncation to width).
        std::string shExpr = isUnsigned ? "bitcast<i32>(bi)" : "bi";
        std::string zero = isUnsigned ? "0u" : "0";
        body = "        let sh = " + shExpr
            + ";\n"
              "        if (sh >= 0) {\n"
              "            if (sh >= 32) { res = "
            + zero
            + "; } else { res = ai << u32(sh); }\n"
              "        } else {\n"
              "            res = ai >> min(u32(-sh), 31u);\n        }\n";
    } else {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    (void)MAXV;
    (void)MINV;
    (void)T;
    return runInt32Binary(a, b, intType, "gpuArray:i32:" + op + ":" + intType, body);
}
//=============================================================================
ArrayOf
GpuInt32Unary(GpuArrayObject* a, const std::string& op)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    std::string intType = a->underlyingType();
    bool isUnsigned = (intType == "uint32");
    const char* T = isUnsigned ? "u32" : "i32";

    std::string body;
    if (op == "uminus") {
        if (isUnsigned) {
            body = "        res = 0u;\n";
        } else {
            body = "        if (ai == " WGSL_I32_MIN ") { res = " WGSL_I32_MAX "; }\n"
                   "        else { res = -ai; }\n";
        }
    } else if (op == "abs") {
        if (isUnsigned) {
            body = "        res = ai;\n";
        } else {
            body = "        if (ai == " WGSL_I32_MIN ") { res = " WGSL_I32_MAX "; }\n"
                   "        else { res = abs(ai); }\n";
        }
    } else if (op == "sign") {
        if (isUnsigned) {
            body = "        if (ai > 0u) { res = 1u; } else { res = 0u; }\n";
        } else {
            body = "        if (ai > 0) { res = 1; } else if (ai < 0) { res = -1; } else { res = "
                   "0; }\n";
        }
    } else {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }

    std::string source
        = std::string("struct Params { n : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n"
                      "    if (i < params.n) {\n"
                      "        let ai = bitcast<")
        + T + ">(a[i]);\n        var res : " + T + " = " + T + "(0);\n" + body
        + "        c[i] = bitcast<f32>(res);\n    }\n}\n";

    WGPUComputePipeline pipeline
        = context->pipelineFor("gpuArray:i32u:" + op + ":" + intType, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    Dimensions dims = a->dimensions();
    GpuArrayObject* output = AllocateGpuArray(dims, intType, false);
    uint32_t elementCount = (uint32_t)dims.elementCount();
    if (elementCount > 0) {
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuInt32Compare(GpuArrayObject* a, GpuArrayObject* b, const std::string& cmp)
{
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    std::string intType = GpuInt32ResultType(a, b);
    std::string cmpOp;
    if (cmp == "eq") {
        cmpOp = "==";
    } else if (cmp == "ne") {
        cmpOp = "!=";
    } else if (cmp == "lt") {
        cmpOp = "<";
    } else if (cmp == "le") {
        cmpOp = "<=";
    } else if (cmp == "gt") {
        cmpOp = ">";
    } else if (cmp == "ge") {
        cmpOp = ">=";
    } else {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    // the comparison result is logical (an ordinary f32 0/1 buffer)
    std::string body = "        c[i] = f32(ai " + cmpOp + " bi);\n";
    return runInt32Binary(a, b, "logical", "gpuArray:i32cmp:" + cmp + ":" + intType, body);
}
//=============================================================================
void
GpuRequireNonNegativeExponent(GpuArrayObject* b)
{
    if (b == nullptr || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    ArrayOf host = GatherGpuArray(b);
    host.promoteType(NLS_SINGLE);
    const single* v = (const single*)host.dataPointer();
    indexType n = host.dimensions().elementCount();
    for (indexType k = 0; k < n; ++k) {
        if (v[k] < 0.0f) {
            Error(L"Nelson:power:invalidIntegerExponent",
                L"Integers can only be raised to positive integral powers.");
        }
    }
}
//=============================================================================
ArrayOf
GpuInt32Power(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    GpuRequireNonNegativeExponent(b);
    std::string intType = GpuInt32ResultType(a, b);
    bool isUnsigned = (intType == "uint32");

    // ai = base, bi = exponent (>= 0). Small integer bases (|base| <= 1) are
    // constant-time; otherwise repeated saturating multiply, which reaches a
    // bound within ~31 steps for a 32-bit result, so the loop breaks early.
    // Overflow is tested BEFORE the multiply, by dividing the class bound by the
    // (magnitude of the) base: `(a*b)/b != a` gets optimized away by the WGSL
    // compiler, so it cannot be used to detect overflow here.
    std::string body;
    if (isUnsigned) {
        body = "        if (bi == 0u) { res = 1u; }\n"
               "        else if (ai == 0u) { res = 0u; }\n"
               "        else if (ai == 1u) { res = 1u; }\n"
               "        else {\n"
               "            var acc : u32 = 1u;\n"
               "            var k : u32 = 0u;\n"
               "            loop {\n"
               "                if (k >= bi) { break; }\n"
               "                if (acc > " WGSL_U32_MAX " / ai) { acc = " WGSL_U32_MAX
               "; break; }\n"
               "                acc = acc * ai;\n"
               "                k = k + 1u;\n"
               "            }\n"
               "            res = acc;\n"
               "        }\n";
    } else {
        body = "        if (bi == 0) { res = 1; }\n"
               "        else if (ai == 0) { res = 0; }\n"
               "        else if (ai == 1) { res = 1; }\n"
               "        else if (ai == -1) { if ((bi & 1) == 1) { res = -1; } else { res = 1; } }\n"
               "        else {\n"
               "            var acc : i32 = 1;\n"
               "            var k : i32 = 0;\n"
               "            loop {\n"
               "                if (k >= bi) { break; }\n"
               "                var uacc : u32 = bitcast<u32>(acc); if (acc < 0) { uacc = 0u - "
               "uacc; }\n"
               "                var uai : u32 = bitcast<u32>(ai); if (ai < 0) { uai = 0u - uai; }\n"
               "                var over = false;\n"
               "                if (uacc > 2147483648u / uai) { over = true; }\n"
               "                else {\n"
               "                    let mag = uacc * uai;\n"
               "                    var limit : u32 = 2147483647u;\n"
               "                    if ((acc ^ ai) < 0) { limit = 2147483648u; }\n"
               "                    if (mag > limit) { over = true; }\n"
               "                }\n"
               "                if (over) {\n"
               "                    if (ai < 0 && (bi & 1) == 1) { acc = " WGSL_I32_MIN "; }\n"
               "                    else { acc = " WGSL_I32_MAX "; }\n"
               "                    break;\n"
               "                }\n"
               "                acc = acc * ai;\n"
               "                k = k + 1;\n"
               "            }\n"
               "            res = acc;\n"
               "        }\n";
    }
    return runInt32Binary(a, b, intType, "gpuArray:i32:pow:" + intType, body);
}
//=============================================================================
// static helper functions
//=============================================================================
// Materialize `a` in the given 32-bit integer storage. When `a` already uses
// that class it is returned as-is (owned = false); otherwise it is a real
// (single/logical) operand and is converted with a rounding + saturating cast
// into a fresh int32/uint32 gpuArray (owned = true, caller deletes it).
static GpuArrayObject*
materializeInt(GpuArrayObject* a, const std::string& intType, bool& owned)
{
    owned = false;
    if (a->underlyingType() == intType) {
        return a;
    }
    GpuContext* context = GpuContext::getInstance();
    bool isUnsigned = (intType == "uint32");
    const char* T = isUnsigned ? "u32" : "i32";
    std::string clamp;
    if (isUnsigned) {
        clamp = "        if (r <= 0.0) { res = 0u; }\n"
                "        else if (r >= 4294967295.0) { res = " WGSL_U32_MAX "; }\n"
                "        else { res = u32(r); }\n";
    } else {
        clamp = "        if (r >= 2147483647.0) { res = " WGSL_I32_MAX "; }\n"
                "        else if (r <= -2147483648.0) { res = " WGSL_I32_MIN "; }\n"
                "        else { res = i32(r); }\n";
    }
    // f32 value -> rounded (half away from zero) -> saturated -> raw int bits
    std::string source
        = std::string("struct Params { n : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n"
                      "    if (i < params.n) {\n"
                      "        let x = a[i];\n"
                      "        let r = sign(x) * floor(abs(x) + 0.5);\n"
                      "        var res : ")
        + T + " = " + T + "(0);\n" + clamp + "        c[i] = bitcast<f32>(res);\n    }\n}\n";

    WGPUComputePipeline pipeline
        = context->pipelineFor(std::string("gpuArray:tocast:") + intType, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    Dimensions dims = a->dimensions();
    GpuArrayObject* output = AllocateGpuArray(dims, intType, false);
    uint32_t elementCount = (uint32_t)dims.elementCount();
    if (elementCount > 0) {
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    owned = true;
    return output;
}
//=============================================================================
// Shared binary dispatch: coerce both operands to `intType` storage, handle
// scalar broadcast, run `body` (which reads i32/u32 `ai`,`bi` and writes `res`,
// or writes `c[i]` directly for a comparison), and produce a `resultType` array.
static ArrayOf
runInt32Binary(GpuArrayObject* a, GpuArrayObject* b, const std::string& intType,
    const std::string& opKey, const std::string& body)
{
    GpuContext* context = GpuContext::getInstance();
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    bool comparison = (intType == "logical");
    std::string storageType = comparison ? GpuInt32ResultType(a, b) : intType;
    bool isUnsigned = (storageType == "uint32");
    const char* T = isUnsigned ? "u32" : "i32";

    bool ownA = false;
    bool ownB = false;
    GpuArrayObject* ao = materializeInt(a, storageType, ownA);
    GpuArrayObject* bo = materializeInt(b, storageType, ownB);

    bool aScalar = ao->dimensions().isScalar();
    bool bScalar = bo->dimensions().isScalar();
    Dimensions outputDims;
    uint32_t aStride = 1;
    uint32_t bStride = 1;
    bool sizeError = false;
    if (aScalar && !bScalar) {
        outputDims = bo->dimensions();
        aStride = 0;
    } else if (bScalar && !aScalar) {
        outputDims = ao->dimensions();
        bStride = 0;
    } else {
        if (!ao->dimensions().equals(bo->dimensions())) {
            sizeError = true;
        }
        outputDims = ao->dimensions();
    }
    if (sizeError) {
        if (ownA) {
            delete ao;
        }
        if (ownB) {
            delete bo;
        }
        Error(
            L"Nelson:gpuArray:sizeMismatch", L"Arrays have incompatible sizes for this operation.");
    }

    // comparison writes the logical result straight into c; arithmetic writes
    // res (an i32/u32) and stores its bits.
    std::string prologue = std::string("        let ai = bitcast<") + T
        + ">(a[i * params.aStride]);\n        let bi = bitcast<" + T
        + ">(b[i * params.bStride]);\n";
    std::string body2 = body;
    std::string declRes;
    std::string epilogue;
    if (!comparison) {
        declRes = std::string("        var res : ") + T + " = " + T + "(0);\n";
        epilogue = "        c[i] = bitcast<f32>(res);\n";
    }
    std::string source
        = std::string("struct Params { n : u32, aStride : u32, bStride : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
                      "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(3) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n"
                      "    if (i < params.n) {\n")
        + prologue + declRes + body2 + epilogue + "    }\n}\n";

    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        if (ownA) {
            delete ao;
        }
        if (ownB) {
            delete bo;
        }
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output
        = AllocateGpuArray(outputDims, comparison ? "logical" : storageType, false);
    uint32_t elementCount = (uint32_t)outputDims.elementCount();
    if (elementCount > 0) {
        uint32_t params[3] = { elementCount, aStride, bStride };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(pipeline, { ao->buffer(), bo->buffer(), output->buffer() }, params, 3,
            workgroupCountX, 1);
    }
    if (ownA) {
        delete ao;
    }
    if (ownB) {
        delete bo;
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
