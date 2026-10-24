//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "Dimensions.hpp"
#include "GpuArrayObject.hpp"
#include "nlsGpu_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Generic elementwise binary operation on the device. Handles same-size
 * operands and scalar broadcast in both directions (via read strides).
 * `opKey` names the cached pipeline; `wgslExpr` is a WGSL f32 expression
 * using `ai` (left element) and `bi` (right element). Result stays device
 * resident.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuBinaryElementwise(GpuArrayObject* a, GpuArrayObject* b, const std::string& opKey,
    const std::string& wgslExpr, const std::string& resultType = "single");
//=============================================================================
/**
 * Generic elementwise unary operation on the device. `wgslExpr` is a WGSL
 * f32 expression using `ai` (the input element). Result stays device resident.
 */
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuUnaryElementwise(GpuArrayObject* a, const std::string& opKey, const std::string& wgslExpr,
    const std::string& resultType = "single");
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuPlus(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMinus(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTimes(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRdivide(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLdivide(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuPower(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuUminus(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAbs(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSqrt(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuExp(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLog(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSin(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCos(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTan(GpuArrayObject* a);
//=============================================================================
// Rounding and sign (elementwise). floor/ceil/round/fix keep an integer input
// unchanged (identity); round uses round-half-away-from-zero. Real inputs give
// a single result; sign keeps the integer class.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFloor(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCeil(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRound(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuFix(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSign(GpuArrayObject* a);
//=============================================================================
// Bitwise operations on integer gpuArray (bitand/bitor/bitxor/bitshift).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuBitand(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuBitor(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuBitxor(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuBitshift(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
// Modulo and remainder (elementwise). mod follows the sign of the divisor
// (mod(x,0) == x); rem follows the sign of the dividend (rem(x,0) == NaN).
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMod(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRem(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
// Additional real elementwise math functions (single result). Complex and
// 32-bit-integer inputs raise a clear error.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSinh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCosh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuTanh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAsin(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAcos(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAtan(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLog2(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLog10(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAtan2(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuHypot(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
// More real elementwise math (single result): reciprocal trigonometry, inverse
// hyperbolic, exp/log variants, angle conversions. Same complex/int rules.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuExpm1(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuLog1p(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuPow2(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSec(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCsc(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCot(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAsec(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAcsc(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAcot(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuSech(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCsch(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuCoth(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAsinh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAcosh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuAtanh(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuDeg2rad(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuRad2deg(GpuArrayObject* a);
//=============================================================================
// Elementwise predicates (logical result). Integer inputs cannot be NaN/Inf,
// so isnan/isinf are all-false and isfinite all-true for them.
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIsNaN(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIsInf(GpuArrayObject* a);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuIsFinite(GpuArrayObject* a);
//=============================================================================
// Binary elementwise max/min (max(A,B) / min(A,B))
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMaxBinary(GpuArrayObject* a, GpuArrayObject* b);
NLSGPU_ENGINE_IMPEXP ArrayOf
GpuMinBinary(GpuArrayObject* a, GpuArrayObject* b);
//=============================================================================
} // namespace Nelson
//=============================================================================
