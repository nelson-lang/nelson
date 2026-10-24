//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// Real elementwise math functions (transcendental, rounding, mod/rem) built on
// the shared GpuUnaryElementwise / GpuBinaryElementwise dispatchers. Complex
// inputs route to the GpuComplex* kernels; integer rounding keeps the class.
//=============================================================================
#include "GpuKernels.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
GpuSqrt(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexSqrt(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:sqrt", "sqrt(ai)");
}
//=============================================================================
ArrayOf
GpuExp(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexExp(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:exp", "exp(ai)");
}
//=============================================================================
ArrayOf
GpuLog(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexLog(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:log", "log(ai)");
}
//=============================================================================
ArrayOf
GpuSin(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexSin(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:sin", "sin(ai)");
}
//=============================================================================
ArrayOf
GpuCos(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexCos(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:cos", "cos(ai)");
}
//=============================================================================
ArrayOf
GpuTan(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexTan(a);
    }
    return GpuUnaryElementwise(a, "gpuArray:tan", "tan(ai)");
}
//=============================================================================
// Rounding functions. An integer input is returned unchanged (floor/ceil/round/
// fix of an integer is itself), which also keeps the integer class.
//=============================================================================
ArrayOf
GpuFloor(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexFloor(a);
    }
    if (a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    return GpuUnaryElementwise(a, "gpuArray:floor", "floor(ai)");
}
//=============================================================================
ArrayOf
GpuCeil(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexCeil(a);
    }
    if (a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    return GpuUnaryElementwise(a, "gpuArray:ceil", "ceil(ai)");
}
//=============================================================================
ArrayOf
GpuRound(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexRound(a);
    }
    if (a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    // round half away from zero (WGSL round() rounds half to even)
    return GpuUnaryElementwise(a, "gpuArray:round", "sign(ai) * floor(abs(ai) + 0.5)");
}
//=============================================================================
ArrayOf
GpuFix(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexFix(a);
    }
    if (a->isRawIntStorage() || !GpuIntegerResultType(a, a).empty()) {
        return ArrayOf::handle(CloneGpuArray(a));
    }
    return GpuUnaryElementwise(a, "gpuArray:fix", "trunc(ai)");
}
//=============================================================================
ArrayOf
GpuSign(GpuArrayObject* a)
{
    if (a->isComplex()) {
        return GpuComplexSign(a);
    }
    if (a->isRawIntStorage()) {
        return GpuInt32Unary(a, "sign");
    }
    std::string intType = GpuIntegerResultType(a, a);
    if (!intType.empty()) {
        return GpuUnaryElementwise(a, "gpuArray:sign:" + intType, "sign(ai)", intType);
    }
    return GpuUnaryElementwise(a, "gpuArray:sign", "sign(ai)");
}
//=============================================================================
ArrayOf
GpuMod(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:GPU:argumentNotReal", L"Argument must be real.");
    }
    // mod(x, y) = x - floor(x / y) * y, with mod(x, 0) == x. The (bi == 0) mask
    // avoids the division by zero while forcing the subtracted term to 0.
    const char* expr = "ai - floor(ai / (bi + f32(bi == 0.0))) * bi";
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(a, b, "gpuArray:mod:" + intType, expr, intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:mod", expr);
}
//=============================================================================
ArrayOf
GpuRem(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:GPU:argumentNotReal", L"Argument must be real.");
    }
    // rem(x, y) = x - fix(x / y) * y, with rem(x, 0) == NaN (produced naturally
    // by the division by zero).
    const char* expr = "ai - trunc(ai / bi) * bi";
    std::string intType = GpuIntegerResultType(a, b);
    if (!intType.empty()) {
        return GpuBinaryElementwise(a, b, "gpuArray:rem:" + intType, expr, intType);
    }
    return GpuBinaryElementwise(a, b, "gpuArray:rem", expr);
}
//=============================================================================
// Additional real elementwise math functions. GpuUnaryElementwise already
// rejects complex and 32-bit-integer inputs; results are single.
//=============================================================================
ArrayOf
GpuSinh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:sinh", "sinh(ai)");
}
//=============================================================================
ArrayOf
GpuCosh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:cosh", "cosh(ai)");
}
//=============================================================================
ArrayOf
GpuTanh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:tanh", "tanh(ai)");
}
//=============================================================================
ArrayOf
GpuAsin(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:asin", "asin(ai)");
}
//=============================================================================
ArrayOf
GpuAcos(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:acos", "acos(ai)");
}
//=============================================================================
ArrayOf
GpuAtan(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:atan", "atan(ai)");
}
//=============================================================================
ArrayOf
GpuLog2(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:log2", "log2(ai)");
}
//=============================================================================
ArrayOf
GpuLog10(GpuArrayObject* a)
{
    // WGSL has no log10; use the natural log ratio.
    return GpuUnaryElementwise(a, "gpuArray:log10", "log(ai) / log(10.0)");
}
//=============================================================================
ArrayOf
GpuAtan2(GpuArrayObject* a, GpuArrayObject* b)
{
    return GpuBinaryElementwise(a, b, "gpuArray:atan2", "atan2(ai, bi)");
}
//=============================================================================
ArrayOf
GpuHypot(GpuArrayObject* a, GpuArrayObject* b)
{
    return GpuBinaryElementwise(a, b, "gpuArray:hypot", "sqrt(ai * ai + bi * bi)");
}
//=============================================================================
// Reciprocal trigonometry, inverse hyperbolic, exp/log variants and angle
// conversions. All single-result; GpuUnaryElementwise rejects complex and
// 32-bit-integer inputs with a clear error.
//=============================================================================
ArrayOf
GpuExpm1(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:expm1", "exp(ai) - 1.0");
}
//=============================================================================
ArrayOf
GpuLog1p(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:log1p", "log(1.0 + ai)");
}
//=============================================================================
ArrayOf
GpuPow2(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:pow2", "exp2(ai)");
}
//=============================================================================
ArrayOf
GpuSec(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:sec", "1.0 / cos(ai)");
}
//=============================================================================
ArrayOf
GpuCsc(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:csc", "1.0 / sin(ai)");
}
//=============================================================================
ArrayOf
GpuCot(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:cot", "cos(ai) / sin(ai)");
}
//=============================================================================
ArrayOf
GpuAsec(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:asec", "acos(1.0 / ai)");
}
//=============================================================================
ArrayOf
GpuAcsc(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:acsc", "asin(1.0 / ai)");
}
//=============================================================================
ArrayOf
GpuAcot(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:acot", "atan(1.0 / ai)");
}
//=============================================================================
ArrayOf
GpuSech(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:sech", "1.0 / cosh(ai)");
}
//=============================================================================
ArrayOf
GpuCsch(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:csch", "1.0 / sinh(ai)");
}
//=============================================================================
ArrayOf
GpuCoth(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:coth", "cosh(ai) / sinh(ai)");
}
//=============================================================================
ArrayOf
GpuAsinh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:asinh", "asinh(ai)");
}
//=============================================================================
ArrayOf
GpuAcosh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:acosh", "acosh(ai)");
}
//=============================================================================
ArrayOf
GpuAtanh(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:atanh", "atanh(ai)");
}
//=============================================================================
ArrayOf
GpuDeg2rad(GpuArrayObject* a)
{
    // pi / 180
    return GpuUnaryElementwise(a, "gpuArray:deg2rad", "ai * 0.017453292519943295");
}
//=============================================================================
ArrayOf
GpuRad2deg(GpuArrayObject* a)
{
    // 180 / pi
    return GpuUnaryElementwise(a, "gpuArray:rad2deg", "ai * 57.29577951308232");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
