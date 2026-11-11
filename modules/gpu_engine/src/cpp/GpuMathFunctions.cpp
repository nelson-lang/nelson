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
#include "GpuElementwiseMath.hpp"
#include "GpuTransfer.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
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
    // NaN keeps its value (the hardware sign gives 0)
    return GpuUnaryElementwise(
        a, "gpuArray:sign", "select(sign(ai), ai, (bitcast<u32>(ai) & 0x7fffffffu) > 0x7f800000u)");
}
//=============================================================================
ArrayOf
GpuMod(GpuArrayObject* a, GpuArrayObject* b)
{
    if (a->isComplex() || b->isComplex()) {
        Error(L"Nelson:GPU:argumentNotReal", L"Argument must be real.");
    }
    // real operands: quotients within rounding of an integer count as that integer
    ArrayOf result;
    if (GpuElementwiseMath("mod", std::vector<GpuArrayObject*> { a, b }, result)) {
        return result;
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
    // real operands: quotients within rounding of an integer count as that integer
    ArrayOf result;
    if (GpuElementwiseMath("rem", std::vector<GpuArrayObject*> { a, b }, result)) {
        return result;
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
// Reciprocal trigonometry, inverse hyperbolic, exp/log variants and angle
// conversions. All single-result; GpuUnaryElementwise rejects complex and
// 32-bit-integer inputs with a clear error.
//=============================================================================
ArrayOf
GpuPow2(GpuArrayObject* a)
{
    return GpuUnaryElementwise(a, "gpuArray:pow2", "exp2(ai)");
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
