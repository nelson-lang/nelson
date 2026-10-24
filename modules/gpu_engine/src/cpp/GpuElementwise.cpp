//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
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
// Small integer classes (int8/16, uint8/16) reuse the f32 kernels: values fit
// exactly in f32 and the result expression is wrapped by GpuSaturateExpr
// (round half away from zero + clamp) so it matches host integer arithmetic.
// GpuIntegerResultType / GpuSaturateExpr live in GpuArrayObject.cpp so the
// reduction kernels share them.
//=============================================================================
ArrayOf
GpuBinaryElementwise(GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey,
    const std::string& wgslExpr, const std::string& resultType)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || b == nullptr || !a->isValid() || !b->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    bool aScalar = a->dimensions().isScalar();
    bool bScalar = b->dimensions().isScalar();

    Dimensions outputDims;
    uint32_t aStride = 1;
    uint32_t bStride = 1;
    if (aScalar && !bScalar) {
        outputDims = b->dimensions();
        aStride = 0;
        bStride = 1;
    } else if (bScalar && !aScalar) {
        outputDims = a->dimensions();
        aStride = 1;
        bStride = 0;
    } else {
        if (!a->dimensions().equals(b->dimensions())) {
            Error(L"Nelson:gpuArray:sizeMismatch",
                L"Arrays have incompatible sizes for this operation.");
        }
        outputDims = a->dimensions();
    }

    std::string source = GpuBuildBinaryWgsl(wgslExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    GpuArrayObject* output = AllocateGpuArray(outputDims, resultType);
    uint32_t elementCount = (uint32_t)outputDims.elementCount();
    if (elementCount > 0) {
        uint32_t params[3] = { elementCount, aStride, bStride };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(pipeline, { a->buffer(), b->buffer(), output->buffer() }, params, 3,
            workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
ArrayOf
GpuUnaryElementwise(GpuArrayObject* a, const std::string& opKey, const std::string& wgslExpr,
    const std::string& resultType)
{
    GpuContext* context = GpuContext::getInstance();
    if (a == nullptr || !a->isValid()) {
        Error(L"Nelson:gpuArray:invalidHandle", L"Invalid gpuArray handle.");
    }
    if (a->isComplex()) {
        Error(L"Nelson:gpuArray:complexUnsupported",
            L"This operation is not supported for complex gpuArray yet.");
    }
    if (a->isRawIntStorage()) {
        Error(L"Nelson:gpuArray:int32Unsupported",
            L"This operation is not supported for int32 gpuArray yet.");
    }
    if (!context->isAvailable()) {
        Error(L"Nelson:GPU:noDeviceAvailable", L"No compatible GPU device found.");
    }
    std::string source = GpuBuildUnaryWgsl(wgslExpr);
    WGPUComputePipeline pipeline = context->pipelineFor(opKey, source.c_str(), "main");
    if (pipeline == nullptr) {
        Error(L"Nelson:GPU:kernelCompilationFailed", L"Cannot compile GPU kernel.");
    }
    Dimensions outputDims = a->dimensions();
    GpuArrayObject* output = AllocateGpuArray(outputDims, resultType);
    uint32_t elementCount = (uint32_t)outputDims.elementCount();
    if (elementCount > 0) {
        uint32_t params[1] = { elementCount };
        uint32_t workgroupCountX = (elementCount + 255u) / 256u;
        GpuDispatchKernel(
            pipeline, { a->buffer(), output->buffer() }, params, 1, workgroupCountX, 1);
    }
    return ArrayOf::handle(output);
}
//=============================================================================
// Binary operators
//=============================================================================
ArrayOf
GpuPlus(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "plus");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexAdd(a, b);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:plus:" + intType, GpuSaturateExpr("ai + bi", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:plus", "ai + bi");
}
//=============================================================================
ArrayOf
GpuMinus(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "minus");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexSub(a, b);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:minus:" + intType, GpuSaturateExpr("ai - bi", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:minus", "ai - bi");
}
//=============================================================================
ArrayOf
GpuTimes(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "times");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexMul(a, b);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:times:" + intType, GpuSaturateExpr("ai * bi", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:times", "ai * bi");
}
//=============================================================================
ArrayOf
GpuRdivide(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "div");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexDiv(a, b);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:rdivide:" + intType, GpuSaturateExpr("ai / bi", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:rdivide", "ai / bi");
}
//=============================================================================
ArrayOf
GpuLdivide(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(b, a, "div"); // a \ b == b / a
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexDiv(b, a); // a \ b == b / a
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:ldivide:" + intType, GpuSaturateExpr("bi / ai", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:ldivide", "bi / ai");
}
//=============================================================================
ArrayOf
GpuPower(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Power(a, b);
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexPower(a, b);
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        // integer power: exponent must be non-negative; results are small enough
        // to be exact in f32, so compute the magnitude with pow(|base|, exp) and
        // restore the sign (negative base with an odd exponent).
        GpuRequireNonNegativeExponent(b);
        std::string expr
            = "(1.0 - 2.0 * f32((ai < 0.0) && (fract(bi * 0.5) != 0.0))) * pow(abs(ai), bi)";
        return GpuBinaryElementwise(
            a, b, "gpuArray:power:" + intType, GpuSaturateExpr(expr, intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:power", "pow(ai, bi)");
}
//=============================================================================
ArrayOf
GpuMaxBinary(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "max");
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:maxbin:" + intType, GpuSaturateExpr("max(ai, bi)", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:maxbin", "max(ai, bi)");
}
//=============================================================================
ArrayOf
GpuMinBinary(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Binary(a, b, "min");
    }
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(
            a, b, "gpuArray:minbin:" + intType, GpuSaturateExpr("min(ai, bi)", intType), intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:minbin", "min(ai, bi)");
}
//=============================================================================
// Unary operators / elementwise math functions
//=============================================================================
ArrayOf
GpuUminus(GpuArrayObject* a)
{
    if (a->isRawIntStorage()) {
        return GpuInt32Unary(a, "uminus");
    }
    if (a->isComplex()) {
        return GpuComplexNegate(a);
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return GpuUnaryElementwise(
            a, "gpuArray:uminus:" + intType, GpuSaturateExpr("-ai", intType), intType);
    }
    return GpuUnaryElementwise(a, "gpuArray:uminus", "-ai");
}
//=============================================================================
ArrayOf
GpuAbs(GpuArrayObject* a)
{
    if (a->isRawIntStorage()) {
        return GpuInt32Unary(a, "abs");
    }
    if (a->isComplex()) {
        return GpuComplexAbs(a);
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return GpuUnaryElementwise(
            a, "gpuArray:abs:" + intType, GpuSaturateExpr("abs(ai)", intType), intType);
    }
    return GpuUnaryElementwise(a, "gpuArray:abs", "abs(ai)");
}
//=============================================================================
// Relational and logical operators (logical result: f32 buffer of 0.0/1.0
// tagged "logical" so gather() rebuilds a logical host array)
//=============================================================================
ArrayOf
GpuEq(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "eq");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexEq(a, b);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:eq", "f32(ai == bi)", "logical");
}
//=============================================================================
ArrayOf
GpuNe(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "ne");
    }
    if (a->isComplex() || b->isComplex()) {
        return GpuComplexNe(a, b);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:ne", "f32(ai != bi)", "logical");
}
//=============================================================================
ArrayOf
GpuLt(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "lt");
    }
    return GpuBinaryElementwise(a, b, "gpuArray:lt", "f32(ai < bi)", "logical");
}
//=============================================================================
ArrayOf
GpuLe(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "le");
    }
    return GpuBinaryElementwise(a, b, "gpuArray:le", "f32(ai <= bi)", "logical");
}
//=============================================================================
ArrayOf
GpuGt(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "gt");
    }
    return GpuBinaryElementwise(a, b, "gpuArray:gt", "f32(ai > bi)", "logical");
}
//=============================================================================
ArrayOf
GpuGe(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isRawIntStorage() || b->isRawIntStorage()) {
        return GpuInt32Compare(a, b, "ge");
    }
    return GpuBinaryElementwise(a, b, "gpuArray:ge", "f32(ai >= bi)", "logical");
}
//=============================================================================
ArrayOf
GpuAnd(GpuArrayObject* a, GpuArrayObject* b)
{
    return GpuBinaryElementwise(a, b, "gpuArray:and", "f32((ai != 0.0) && (bi != 0.0))", "logical");
}
//=============================================================================
ArrayOf
GpuOr(GpuArrayObject* a, GpuArrayObject* b)
{
    return GpuBinaryElementwise(a, b, "gpuArray:or", "f32((ai != 0.0) || (bi != 0.0))", "logical");
}
//=============================================================================
ArrayOf
GpuXor(GpuArrayObject* a, GpuArrayObject* b)
{
    return GpuBinaryElementwise(a, b, "gpuArray:xor", "f32((ai != 0.0) != (bi != 0.0))", "logical");
}
//=============================================================================
ArrayOf
GpuNot(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:not", "f32(ai == 0.0)", "logical");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
