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
// Complex arrays are interleaved (re, im) f32 pairs: element k lives at
// buffer[2*k] (re) and buffer[2*k+1] (im).
//=============================================================================
static const char* REAL_TO_COMPLEX_WGSL = R"WGSL(
struct Params { n : u32 };
@group(0) @binding(0) var<storage, read> a : array<f32>;
@group(0) @binding(1) var<storage, read_write> c : array<f32>;
@group(0) @binding(2) var<uniform> params : Params;
@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
    let i = gid.x;
    if (i < params.n) {
        c[2u * i] = a[i];
        c[2u * i + 1u] = 0.0;
    }
}
)WGSL";
//=============================================================================
// `body` runs with locals ar, ai, br, bi in scope and must write the result to
// c[2u * i] (re) and c[2u * i + 1u] (im). Allows intermediates (e.g. cpow).
static std::string
buildComplexBinaryWgsl(const std::string& body, bool expand)
{
    return GpuMathHelpersWgsl() + GpuBinaryParamsWgsl(expand)
        + "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
          "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
          "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
          "@group(0) @binding(3) var<uniform> params : Params;\n"
          "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i < params.n) {\n"
          "        let ix = operandIndex(i);\n"
          "        let ia = ix.x;\n"
          "        let ib = ix.y;\n"
          "        let ar = a[2u * ia]; let ai = a[2u * ia + 1u];\n"
          "        let br = b[2u * ib]; let bi = b[2u * ib + 1u];\n"
        + body + "\n    }\n}\n";
}
//=============================================================================
static std::string
complexAssign(const std::string& crExpr, const std::string& ciExpr)
{
    return "        c[2u * i] = " + crExpr + ";\n        c[2u * i + 1u] = " + ciExpr + ";";
}
//=============================================================================
// Complex binary comparison -> real (logical) output: c[i] = <expr(ar,ai,br,bi)>.
static std::string
buildComplexCompareWgsl(const std::string& expr, bool expand)
{
    return GpuMathHelpersWgsl() + GpuBinaryParamsWgsl(expand)
        + "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
          "@group(0) @binding(1) var<storage, read> b : array<f32>;\n"
          "@group(0) @binding(2) var<storage, read_write> c : array<f32>;\n"
          "@group(0) @binding(3) var<uniform> params : Params;\n"
          "@compute @workgroup_size(256)\n"
          "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
          "    let i = gid.x;\n"
          "    if (i < params.n) {\n"
          "        let ix = operandIndex(i);\n"
          "        let ia = ix.x;\n"
          "        let ib = ix.y;\n"
          "        let ar = a[2u * ia]; let ai = a[2u * ia + 1u];\n"
          "        let br = b[2u * ib]; let bi = b[2u * ib + 1u];\n"
          "        c[i] = "
        + expr + ";\n    }\n}\n";
}
//=============================================================================
static std::string
buildComplexUnaryWgsl(const std::string& crExpr, const std::string& ciExpr)
{
    return GpuMathHelpersWgsl()
        + std::string("struct Params { n : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n"
                      "    if (i < params.n) {\n"
                      "        let ar = a[2u * i]; let ai = a[2u * i + 1u];\n"
                      "        c[2u * i] = ")
        + crExpr + ";\n        c[2u * i + 1u] = " + ciExpr + ";\n    }\n}\n";
}
//=============================================================================
static std::string
buildComplexToRealWgsl(const std::string& expr)
{
    return GpuMathHelpersWgsl()
        + std::string("struct Params { n : u32 };\n"
                      "@group(0) @binding(0) var<storage, read> a : array<f32>;\n"
                      "@group(0) @binding(1) var<storage, read_write> c : array<f32>;\n"
                      "@group(0) @binding(2) var<uniform> params : Params;\n"
                      "@compute @workgroup_size(256)\n"
                      "fn main(@builtin(global_invocation_id) gid : vec3<u32>) {\n"
                      "    let i = gid.x;\n"
                      "    if (i < params.n) {\n"
                      "        let ar = a[2u * i]; let ai = a[2u * i + 1u];\n"
                      "        c[i] = ")
        + expr + ";\n    }\n}\n";
}
//=============================================================================
static ArrayOf
complexBinary(
    GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey, const std::string& body);
static ArrayOf
complexCompare(
    GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey, const std::string& expr);
static ArrayOf
complexUnary(GpuArrayObject* a, const std::string& opKey, const std::string& crExpr,
    const std::string& ciExpr);
static ArrayOf
complexToReal(GpuArrayObject* a, const std::string& opKey, const std::string& expr,
    const std::string& resultType = "single");
//=============================================================================
ArrayOf
GpuComplexAdd(GpuArrayObject* a, GpuArrayObject* b)
{
    return complexBinary(a, b, "gpuArray:cadd", complexAssign("ar + br", "ai + bi"));
}
//=============================================================================
ArrayOf
GpuComplexSub(GpuArrayObject* a, GpuArrayObject* b)
{
    return complexBinary(a, b, "gpuArray:csub", complexAssign("ar - br", "ai - bi"));
}
//=============================================================================
ArrayOf
GpuComplexMul(GpuArrayObject* a, GpuArrayObject* b)
{
    return complexBinary(
        a, b, "gpuArray:cmul", complexAssign("ar * br - ai * bi", "ar * bi + ai * br"));
}
//=============================================================================
ArrayOf
GpuComplexDiv(GpuArrayObject* a, GpuArrayObject* b)
{
    // (ar + i ai) / (br + i bi) = ((ar br + ai bi) + i(ai br - ar bi)) / (br^2 + bi^2)
    return complexBinary(a, b, "gpuArray:cdiv",
        "        let q = nlsCdiv(ar, ai, br, bi);\n" + complexAssign("q.x", "q.y"));
}
//=============================================================================
ArrayOf
GpuComplexPower(GpuArrayObject* a, GpuArrayObject* b)
{
    // a ^ b = exp(b * log(a)), all complex
    // a zero base gives 0 for a positive real exponent and 1 for a zero exponent
    const char* body
        = "        let l = nlsClog(ar, ai);\n"
          "        var p = nlsCexp(br * l.x - bi * l.y, br * l.y + bi * l.x);\n"
          "        if (ar == 0.0 && ai == 0.0 && bi == 0.0) {\n"
          "            p = select(vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 0.0), br == 0.0);\n"
          "            if (br < 0.0) { p = vec2<f32>(nlsInf(), 0.0); }\n"
          "        }\n"
          "        c[2u * i] = p.x;\n"
          "        c[2u * i + 1u] = p.y;";
    return complexBinary(a, b, "gpuArray:cpow", body);
}
//=============================================================================
ArrayOf
GpuComplexEq(GpuArrayObject* a, GpuArrayObject* b)
{
    return complexCompare(a, b, "gpuArray:ceq", "f32((ar == br) && (ai == bi))");
}
//=============================================================================
ArrayOf
GpuComplexNe(GpuArrayObject* a, GpuArrayObject* b)
{
    return complexCompare(a, b, "gpuArray:cne", "f32((ar != br) || (ai != bi))");
}
//=============================================================================
ArrayOf
GpuComplexNegate(GpuArrayObject* a)
{
    return complexUnary(a, "gpuArray:cneg", "-ar", "-ai");
}
//=============================================================================
ArrayOf
GpuComplexExp(GpuArrayObject* a)
{
    // exp(re + i im) = exp(re) * (cos(im) + i sin(im))
    return complexUnary(a, "gpuArray:cexp", "nlsCexp(ar, ai).x", "nlsCexp(ar, ai).y");
}
//=============================================================================
ArrayOf
GpuComplexLog(GpuArrayObject* a)
{
    // log(z) = 0.5 ln(re^2 + im^2) + i atan2(im, re)
    return complexUnary(a, "gpuArray:clog", "nlsClog(ar, ai).x", "nlsClog(ar, ai).y");
}
//=============================================================================
ArrayOf
GpuComplexSqrt(GpuArrayObject* a)
{
    // sqrt(z) = |z|^(1/2) * (cos(theta/2) + i sin(theta/2)), theta = atan2(im, re)
    return complexUnary(a, "gpuArray:csqrt", "nlsCsqrt(ar, ai).x", "nlsCsqrt(ar, ai).y");
}
//=============================================================================
ArrayOf
GpuComplexSin(GpuArrayObject* a)
{
    // sin(re + i im) = sin(re) cosh(im) + i cos(re) sinh(im)
    return complexUnary(a, "gpuArray:csin", "nlsSin(ar) * nlsCosh(ai)", "nlsCos(ar) * nlsSinh(ai)");
}
//=============================================================================
ArrayOf
GpuComplexCos(GpuArrayObject* a)
{
    // cos(re + i im) = cos(re) cosh(im) - i sin(re) sinh(im)
    return complexUnary(
        a, "gpuArray:ccos", "nlsCos(ar) * nlsCosh(ai)", "-nlsSin(ar) * nlsSinh(ai)");
}
//=============================================================================
ArrayOf
GpuComplexTan(GpuArrayObject* a)
{
    // tan(z) = (sin(2re) + i sinh(2im)) / (cos(2re) + cosh(2im))
    return complexUnary(a, "gpuArray:ctan", "nlsCtan(ar, ai).x", "nlsCtan(ar, ai).y");
}
//=============================================================================
ArrayOf
GpuConj(GpuArrayObject* a)
{
    if (!a->isComplex()) {
        return ArrayOf::handle(CloneGpuArray(a)); // conj of a real array is itself
    }
    return complexUnary(a, "gpuArray:cconj", "ar", "-ai");
}
//=============================================================================
ArrayOf
GpuReal(GpuArrayObject* a)
{
    if (!a->isComplex()) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    return complexToReal(a, "gpuArray:creal", "ar");
}
//=============================================================================
ArrayOf
GpuImag(GpuArrayObject* a)
{
    if (!a->isComplex()) {
        return GpuUnaryElementwise(a, "gpuArray:zeros", "ai * 0.0");
    }
    return complexToReal(a, "gpuArray:cimag", "ai");
}
//=============================================================================
ArrayOf
GpuComplexAbs(GpuArrayObject* a)
{
    if (!a->isComplex()) {
        return GpuUnaryElementwise(a, "gpuArray:abs", "abs(ai)");
    }
    return complexToReal(a, "gpuArray:cabs", "nlsHypot(ar, ai)");
}
//=============================================================================
ArrayOf
GpuAngle(GpuArrayObject* a)
{
    if (!a->isComplex()) {
        // angle of a real value: 0 when >= 0, pi when < 0
        return GpuUnaryElementwise(a, "gpuArray:rangle", "f32(ai < 0.0) * 3.1415927");
    }
    return complexToReal(a, "gpuArray:cangle", "nlsAtan2(ai, ar)");
}
//=============================================================================
ArrayOf
GpuComplexFloor(GpuArrayObject* a)
{
    return complexUnary(a, "gpuArray:cfloor", "floor(ar)", "floor(ai)");
}
//=============================================================================
ArrayOf
GpuComplexCeil(GpuArrayObject* a)
{
    return complexUnary(a, "gpuArray:cceil", "ceil(ar)", "ceil(ai)");
}
//=============================================================================
ArrayOf
GpuComplexRound(GpuArrayObject* a)
{
    // round half away from zero, per component
    return complexUnary(
        a, "gpuArray:cround", "sign(ar) * floor(abs(ar) + 0.5)", "sign(ai) * floor(abs(ai) + 0.5)");
}
//=============================================================================
ArrayOf
GpuComplexFix(GpuArrayObject* a)
{
    return complexUnary(a, "gpuArray:cfix", "trunc(ar)", "trunc(ai)");
}
//=============================================================================
ArrayOf
GpuComplexSign(GpuArrayObject* a)
{
    // sign(z) = z / |z|, and sign(0) = 0 (the +f32(...) avoids the 0/0)
    const char* denom = "(sqrt(ar * ar + ai * ai) + f32(ar == 0.0 && ai == 0.0))";
    return complexUnary(
        a, "gpuArray:csign", std::string("ar / ") + denom, std::string("ai / ") + denom);
}
//=============================================================================
ArrayOf
GpuComplexIsNaN(GpuArrayObject* a)
{
    return complexToReal(a, "gpuArray:cisnan",
        "f32(((bitcast<u32>(ar) & 0x7fffffffu) > 0x7f800000u) || "
        "((bitcast<u32>(ai) & 0x7fffffffu) > 0x7f800000u))",
        "logical");
}
//=============================================================================
ArrayOf
GpuComplexIsInf(GpuArrayObject* a)
{
    return complexToReal(a, "gpuArray:cisinf",
        "f32(((bitcast<u32>(ar) & 0x7fffffffu) == 0x7f800000u) || "
        "((bitcast<u32>(ai) & 0x7fffffffu) == 0x7f800000u))",
        "logical");
}
//=============================================================================
ArrayOf
GpuComplexIsFinite(GpuArrayObject* a)
{
    return complexToReal(a, "gpuArray:cisfinite",
        "f32(((bitcast<u32>(ar) & 0x7fffffffu) < 0x7f800000u) && "
        "((bitcast<u32>(ai) & 0x7fffffffu) < 0x7f800000u))",
        "logical");
}
//=============================================================================
// static helper functions
//=============================================================================
GpuArrayObject*
GpuPromoteToComplex(GpuArrayObject* a)
{
    // caller owns the returned object (delete it)
    if (a->isComplex()) {
        return CloneGpuArray(a);
    }
    GpuContext* context = GpuContext::getInstance();
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), "single", true);
    uint32_t elementCount = (uint32_t)a->dimensions().elementCount();
    if (elementCount > 0) {
        WGPUComputePipeline pipeline
            = context->pipelineFor("gpuArray:real2complex", REAL_TO_COMPLEX_WGSL, "main");
        if (pipeline == nullptr) {
            delete output;
            Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
        }
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    return output;
}
//=============================================================================
static ArrayOf
complexBinary(
    GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey, const std::string& body)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    GpuBinaryGeometry geometry = GpuComputeBinaryGeometry(a->dimensions(), b->dimensions());
    Dimensions outputDims = geometry.outputDims;
    // both operands must be complex-resident
    GpuArrayObject* aTemp = a->isComplex() ? nullptr : GpuPromoteToComplex(a);
    GpuArrayObject* bTemp = b->isComplex() ? nullptr : GpuPromoteToComplex(b);
    GpuArrayObject* aC = aTemp ? aTemp : a;
    GpuArrayObject* bC = bTemp ? bTemp : b;

    ArrayOf result;
    try {
        std::string source = buildComplexBinaryWgsl(body, geometry.expand);
        GpuArrayObject* output = AllocateGpuArray(outputDims, "single", true);
        result = ArrayOf::handle(output);
        GpuRunBinaryKernel(
            opKey, source, geometry, { aC->buffer(), bC->buffer(), output->buffer() });
    } catch (...) {
        delete aTemp;
        delete bTemp;
        throw;
    }
    delete aTemp;
    delete bTemp;
    return result;
}
//=============================================================================
static ArrayOf
complexCompare(
    GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey, const std::string& expr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    GpuBinaryGeometry geometry = GpuComputeBinaryGeometry(a->dimensions(), b->dimensions());
    Dimensions outputDims = geometry.outputDims;
    GpuArrayObject* aTemp = a->isComplex() ? nullptr : GpuPromoteToComplex(a);
    GpuArrayObject* bTemp = b->isComplex() ? nullptr : GpuPromoteToComplex(b);
    GpuArrayObject* aC = aTemp ? aTemp : a;
    GpuArrayObject* bC = bTemp ? bTemp : b;

    ArrayOf result;
    try {
        std::string source = buildComplexCompareWgsl(expr, geometry.expand);
        // real logical output (one f32 per element)
        GpuArrayObject* output = AllocateGpuArray(outputDims, "logical", false);
        result = ArrayOf::handle(output);
        GpuRunBinaryKernel(
            opKey, source, geometry, { aC->buffer(), bC->buffer(), output->buffer() });
    } catch (...) {
        delete aTemp;
        delete bTemp;
        throw;
    }
    delete aTemp;
    delete bTemp;
    return result;
}
//=============================================================================
static ArrayOf
complexUnary(GpuArrayObject* a, const std::string& opKey, const std::string& crExpr,
    const std::string& ciExpr)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    std::string source = buildComplexUnaryWgsl(crExpr, ciExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), "single", true);
    uint32_t elementCount = (uint32_t)a->dimensions().elementCount();
    if (elementCount > 0) {
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
static ArrayOf
complexToReal(GpuArrayObject* a, const std::string& opKey, const std::string& expr,
    const std::string& resultType)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    std::string source = buildComplexToRealWgsl(expr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(a->dimensions(), resultType, false);
    uint32_t elementCount = (uint32_t)a->dimensions().elementCount();
    if (elementCount > 0) {
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
