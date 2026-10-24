//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "polyval_hornerBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <cmath>
#include <complex>
//=============================================================================
// Forbid FP contraction: the recurrence needs the multiply and the add as two rounded
// steps. Apple clang on arm64 fuses "a * b + c" by default, which would diverge from the
// interpreted "y = y .* x + c" loop. The explicit std::fma calls are unaffected.
#if defined(__clang__) || defined(__GNUC__)
#pragma STDC FP_CONTRACT OFF
#endif
//=============================================================================
// On x86 targets with FMA (the x64 release build) Eigen evaluates the complex multiply
// of the interpreted loop with _mm*_fmaddsub. This macro mirrors Eigen's own
// EIGEN_VECTORIZE_FMA test. Without FMA on x86, Eigen's addsub form and the scalar
// product round identically, so the plain product is used.
#if defined(__FMA__) || defined(__AVX512F__) || (defined(_MSC_VER) && defined(__AVX2__))
#define NLS_POLYVAL_COMPLEX_USES_FMA 1
#else
#define NLS_POLYVAL_COMPLEX_USES_FMA 0
#endif
//=============================================================================
// On ARM64 with the ARMv8.3-A complex extension, Eigen evaluates complex cwiseProduct with
// FCMLA intrinsics (vcmlaq_f32 / vcmlaq_rot90_f32). The rounding pattern differs from x86
// fmaddsub: the first pass computes ar*br and ar*bi as plain multiplies, and the second
// pass fuses the ai terms: fma(-ai, bi, ar*br) and fma(ai, br, ar*bi).
#if defined(__ARM_FEATURE_COMPLEX)
#define NLS_POLYVAL_COMPLEX_USES_FCMLA 1
#else
#define NLS_POLYVAL_COMPLEX_USES_FCMLA 0
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
// Fused Horner evaluation: y(i) = ((c(1)*x(i) + c(2))*x(i) + ...) + c(n).
// Multiply and add stay two rounded steps (never a fused multiply-add), so the result is
// bit-identical to the interpreted "y = y .* x + c" loop.
//=============================================================================
template <typename T>
static ArrayOf
hornerReal(
    NelsonType outputClass, const ArrayOf& coefficientsInput, const ArrayOf& evaluationPointsInput)
{
    ArrayOf coefficients(coefficientsInput);
    ArrayOf evaluationPoints(evaluationPointsInput);
    coefficients.promoteType(outputClass);
    evaluationPoints.promoteType(outputClass);

    const indexType coefficientCount = coefficients.elementCount();
    const indexType pointCount = evaluationPoints.elementCount();
    const T* coefficientData = static_cast<const T*>(coefficients.dataPointer());
    const T* pointData = static_cast<const T*>(evaluationPoints.dataPointer());
    T* resultData
        = static_cast<T*>(ArrayOf::allocateArrayOf(outputClass, pointCount, stringVector(), false));

    const T leadingCoefficient = coefficientData[0];
    OMP_PARALLEL_FOR_LOOP(pointCount)
    for (ompIndexType pointIndex = 0; pointIndex < (ompIndexType)pointCount; ++pointIndex) {
        const T point = pointData[pointIndex];
        T accumulator = leadingCoefficient;
        for (indexType coefficientIndex = 1; coefficientIndex < coefficientCount;
            ++coefficientIndex) {
            accumulator = accumulator * point + coefficientData[coefficientIndex];
        }
        resultData[pointIndex] = accumulator;
    }
    return ArrayOf(outputClass, evaluationPoints.dimensions(), resultData);
}
//=============================================================================
// Reproduces one element of Eigen's FMA complex product (_mm*_fmaddsub):
//   tmp1 = [ai, ai] * [bi, br]; result = fmaddsub([ar, ar], [br, bi], tmp1).
// When the build has no FMA this is never used (the plain product is taken).
template <typename T>
static inline std::complex<T>
fusedComplexProduct(const std::complex<T>& a, const std::complex<T>& b)
{
    const T ar = a.real();
    const T ai = a.imag();
    const T br = b.real();
    const T bi = b.imag();
    const T crossReal = ai * bi;
    const T crossImag = ai * br;
    return std::complex<T>(std::fma(ar, br, -crossReal), std::fma(ar, bi, crossImag));
}
//=============================================================================
// Reproduces one element of Eigen's FCMLA complex product (vcmlaq + vcmlaq_rot90):
//   pass 1: re = ar * br, im = ar * bi (plain multiplies)
//   pass 2: re = fma(-ai, bi, re), im = fma(ai, br, im)
template <typename T>
static inline std::complex<T>
fcmlaComplexProduct(const std::complex<T>& a, const std::complex<T>& b)
{
    const T ar = a.real();
    const T ai = a.imag();
    const T br = b.real();
    const T bi = b.imag();
    const T termReal = ar * br;
    const T termImag = ar * bi;
    return std::complex<T>(std::fma(-ai, bi, termReal), std::fma(ai, br, termImag));
}
//=============================================================================
// With two or more points Eigen evaluates every element with a SIMD packet.
// On x86 (AVX) the tail uses partial-packet segments, so every element sees FMA.
// On ARM64 (NEON) has_packet_segment is false, so Eigen's tail falls back to scalar
// std::complex::operator* for the last element when the count is odd. A lone
// element (pointCount == 1) is not vectorized at all and uses the plain product.
template <typename T>
static ArrayOf
hornerComplex(
    NelsonType outputClass, const ArrayOf& coefficientsInput, const ArrayOf& evaluationPointsInput)
{
    ArrayOf coefficients(coefficientsInput);
    ArrayOf evaluationPoints(evaluationPointsInput);
    coefficients.promoteType(outputClass);
    evaluationPoints.promoteType(outputClass);

    const indexType coefficientCount = coefficients.elementCount();
    const indexType pointCount = evaluationPoints.elementCount();
    const auto* coefficientData
        = reinterpret_cast<const std::complex<T>*>(coefficients.dataPointer());
    const auto* pointData
        = reinterpret_cast<const std::complex<T>*>(evaluationPoints.dataPointer());
    auto* resultData = reinterpret_cast<std::complex<T>*>(
        ArrayOf::allocateArrayOf(outputClass, pointCount, stringVector(), false));

    const bool useFusedProduct
        = (NLS_POLYVAL_COMPLEX_USES_FMA || NLS_POLYVAL_COMPLEX_USES_FCMLA) && pointCount >= 2;
#if NLS_POLYVAL_COMPLEX_USES_FCMLA
    constexpr indexType neonPacketSize = 16 / sizeof(std::complex<T>);
    const ompIndexType fcmlaBound
        = static_cast<ompIndexType>(pointCount - (pointCount % neonPacketSize));
#endif
    const std::complex<T> leadingCoefficient = coefficientData[0];
    OMP_PARALLEL_FOR_LOOP(pointCount)
    for (ompIndexType pointIndex = 0; pointIndex < (ompIndexType)pointCount; ++pointIndex) {
        const std::complex<T> point = pointData[pointIndex];
        std::complex<T> accumulator = leadingCoefficient;
#if NLS_POLYVAL_COMPLEX_USES_FCMLA
        if (useFusedProduct && pointIndex < fcmlaBound) {
            for (indexType coefficientIndex = 1; coefficientIndex < coefficientCount;
                ++coefficientIndex) {
                accumulator
                    = fcmlaComplexProduct(accumulator, point) + coefficientData[coefficientIndex];
            }
        } else
#elif NLS_POLYVAL_COMPLEX_USES_FMA
        if (useFusedProduct) {
            for (indexType coefficientIndex = 1; coefficientIndex < coefficientCount;
                ++coefficientIndex) {
                accumulator
                    = fusedComplexProduct(accumulator, point) + coefficientData[coefficientIndex];
            }
        } else
#endif
        {
            for (indexType coefficientIndex = 1; coefficientIndex < coefficientCount;
                ++coefficientIndex) {
                accumulator = accumulator * point + coefficientData[coefficientIndex];
            }
        }
        resultData[pointIndex] = accumulator;
    }
    return ArrayOf(outputClass, evaluationPoints.dimensions(), resultData);
}
//=============================================================================
ArrayOfVector
Nelson::PolynomialFunctionsGateway::polyval_hornerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 1);

    const ArrayOf& coefficients = argIn[0];
    const ArrayOf& evaluationPoints = argIn[1];

    const bool isComplex = coefficients.isComplex() || evaluationPoints.isComplex();
    const bool isSingle = coefficients.isSingleClass() || evaluationPoints.isSingleClass();

    ArrayOf result;
    if (isComplex) {
        if (isSingle) {
            result = hornerComplex<single>(NLS_SCOMPLEX, coefficients, evaluationPoints);
        } else {
            result = hornerComplex<double>(NLS_DCOMPLEX, coefficients, evaluationPoints);
        }
        if (result.allReal()) {
            result.promoteType(isSingle ? NLS_SINGLE : NLS_DOUBLE);
        }
    } else if (isSingle) {
        result = hornerReal<single>(NLS_SINGLE, coefficients, evaluationPoints);
    } else {
        result = hornerReal<double>(NLS_DOUBLE, coefficients, evaluationPoints);
    }
    return ArrayOfVector { result };
}
//=============================================================================
