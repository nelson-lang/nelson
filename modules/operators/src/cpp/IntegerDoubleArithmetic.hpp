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
#include <algorithm>
#include <cmath>
#include <limits>
#include <type_traits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// integer array combined with a real double scalar: the operation is computed
// exactly, rounded to the nearest integer (half away from zero) and saturated
// to the integer class; NaN gives 0.
//=============================================================================
enum class IntegerDoubleOperation
{
    Add,
    IntegerMinusDouble,
    DoubleMinusInteger
};
//=============================================================================
template <class T>
static bool
isExactIntegerValueOf(double value)
{
    if (!(std::trunc(value) == value) || !std::isfinite(value)) {
        return false;
    }
    const double lower = static_cast<double>(std::numeric_limits<T>::min());
    const double upper = std::ldexp(1.0, std::numeric_limits<T>::digits);
    return value >= lower && value < upper;
}
//=============================================================================
// sign and magnitude: a 64-bit integer and the integer part of a double are
// added without any intermediate rounding
//=============================================================================
struct SignedMagnitude64
{
    bool negative;
    uint64 magnitude;
};
//=============================================================================
static inline SignedMagnitude64
signedMagnitudeOf(int64 value)
{
    if (value < 0) {
        return { true, static_cast<uint64>(0) - static_cast<uint64>(value) };
    }
    return { false, static_cast<uint64>(value) };
}
//=============================================================================
static inline SignedMagnitude64
signedMagnitudeOf(uint64 value)
{
    return { false, value };
}
//=============================================================================
// sum of two sign-magnitude values; false when the magnitude exceeds 64 bits
static inline bool
addSignedMagnitude(SignedMagnitude64 a, SignedMagnitude64 b, SignedMagnitude64& result)
{
    if (a.negative == b.negative) {
        uint64 magnitude = a.magnitude + b.magnitude;
        if (magnitude < a.magnitude) {
            return false;
        }
        result = { a.negative, magnitude };
        return true;
    }
    if (a.magnitude >= b.magnitude) {
        result = { a.negative, a.magnitude - b.magnitude };
    } else {
        result = { b.negative, b.magnitude - a.magnitude };
    }
    if (result.magnitude == 0) {
        result.negative = false;
    }
    return true;
}
//=============================================================================
template <class T>
static inline T
saturateSignedMagnitude(SignedMagnitude64 value)
{
    if constexpr (std::is_signed<T>::value) {
        const uint64 maxMagnitude = static_cast<uint64>(std::numeric_limits<T>::max());
        if (value.negative) {
            return (value.magnitude > maxMagnitude + 1)
                ? std::numeric_limits<T>::min()
                : static_cast<T>(static_cast<uint64>(0) - value.magnitude);
        }
        return (value.magnitude > maxMagnitude) ? std::numeric_limits<T>::max()
                                                : static_cast<T>(value.magnitude);
    } else {
        return value.negative ? static_cast<T>(0) : static_cast<T>(value.magnitude);
    }
}
//=============================================================================
// (negateInteger ? -a : a) + (negateScalar ? -d : d) for a 64-bit class
template <class T>
static T
exactIntegerPlusDouble(T a, bool negateInteger, double d)
{
    if (std::isnan(d)) {
        return static_cast<T>(0);
    }
    SignedMagnitude64 integerPart = signedMagnitudeOf(a);
    if (negateInteger && integerPart.magnitude != 0) {
        integerPart.negative = !integerPart.negative;
    }
    double whole = std::trunc(d);
    double fraction = d - whole;
    if (std::fabs(whole) >= 18446744073709551616.0) {
        // beyond every 64-bit value: the double decides the saturation
        SignedMagnitude64 saturated = { whole < 0, std::numeric_limits<uint64>::max() };
        return saturateSignedMagnitude<T>(saturated);
    }
    SignedMagnitude64 doublePart = { whole < 0, static_cast<uint64>(std::fabs(whole)) };
    SignedMagnitude64 sum;
    if (!addSignedMagnitude(integerPart, doublePart, sum)) {
        sum = { integerPart.negative, std::numeric_limits<uint64>::max() };
        return saturateSignedMagnitude<T>(sum);
    }
    // the fraction moves the magnitude by -1, 0 or +1 (half away from zero)
    if (sum.magnitude == 0) {
        if (std::fabs(fraction) >= 0.5) {
            sum = { fraction < 0, 1 };
        }
    } else {
        double towardsMagnitude = sum.negative ? -fraction : fraction;
        if (towardsMagnitude >= 0.5) {
            sum.magnitude = (sum.magnitude == std::numeric_limits<uint64>::max())
                ? sum.magnitude
                : sum.magnitude + 1;
        } else if (towardsMagnitude < -0.5) {
            sum.magnitude -= 1;
        }
    }
    return saturateSignedMagnitude<T>(sum);
}
//=============================================================================
template <class T, IntegerDoubleOperation operation>
static inline T
integerDoubleValue(T a, double scalar)
{
    if constexpr (operation == IntegerDoubleOperation::Add) {
        return exactIntegerPlusDouble<T>(a, false, scalar);
    } else if constexpr (operation == IntegerDoubleOperation::IntegerMinusDouble) {
        return exactIntegerPlusDouble<T>(a, false, -scalar);
    } else {
        return exactIntegerPlusDouble<T>(a, true, scalar);
    }
}
//=============================================================================
// classes of 32 bits or less: (+-a) + d in integer arithmetic. The scalar is
// split once into an integer offset and a fraction; the fraction only moves
// the result by -1, 0 or +1 (half away from zero), depending on its sign.
//=============================================================================
template <class T>
static void
smallIntegerDoubleLoop(
    const T* ptrA, T* ptrC, indexType Clen, double scalar, IntegerDoubleOperation operation)
{
    if (std::isnan(scalar)) {
        std::fill_n(ptrC, Clen, static_cast<T>(0));
        return;
    }
    const int64 sign = (operation == IntegerDoubleOperation::DoubleMinusInteger) ? -1 : 1;
    const double d = (operation == IntegerDoubleOperation::IntegerMinusDouble) ? -scalar : scalar;
    // 2^40 is beyond every class of 32 bits or less: larger offsets only saturate
    const double bound = 1099511627776.0;
    double whole = std::trunc(d);
    const double fraction = std::isinf(d) ? 0.0 : d - whole;
    whole = std::min(bound, std::max(-bound, whole));
    const int64 offset = static_cast<int64>(whole);
    const int64 adjustPositive = (fraction >= 0.5) ? 1 : ((fraction < -0.5) ? -1 : 0);
    const int64 adjustNegative = (fraction <= -0.5) ? -1 : ((fraction > 0.5) ? 1 : 0);
    const int64 adjustZero = (fraction >= 0.5) ? 1 : ((fraction <= -0.5) ? -1 : 0);
    const int64 lowest = static_cast<int64>(std::numeric_limits<T>::min());
    const int64 highest = static_cast<int64>(std::numeric_limits<T>::max());
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        int64 n = sign * static_cast<int64>(ptrA[k]) + offset;
        n += (n > 0) ? adjustPositive : ((n < 0) ? adjustNegative : adjustZero);
        ptrC[k] = static_cast<T>(std::min(highest, std::max(lowest, n)));
    }
}
//=============================================================================
template <class T, IntegerDoubleOperation operation>
static void
integerDoubleLoop(const T* ptrA, T* ptrC, indexType Clen, double scalar)
{
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = integerDoubleValue<T, operation>(ptrA[k], scalar);
    }
}
//=============================================================================
template <class T>
static ArrayOf
integerDoubleScalarKernel(NelsonType classDestination, const ArrayOf& integerArray, double scalar,
    IntegerDoubleOperation operation)
{
    Dimensions dimsC = integerArray.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    ArrayOf res(classDestination, dimsC, ptrC, false);
    const T* ptrA = (const T*)integerArray.dataPointer();
    if constexpr (sizeof(T) < 8) {
        smallIntegerDoubleLoop<T>(ptrA, ptrC, Clen, scalar, operation);
    } else {
        switch (operation) {
        case IntegerDoubleOperation::Add:
            integerDoubleLoop<T, IntegerDoubleOperation::Add>(ptrA, ptrC, Clen, scalar);
            break;
        case IntegerDoubleOperation::IntegerMinusDouble:
            integerDoubleLoop<T, IntegerDoubleOperation::IntegerMinusDouble>(
                ptrA, ptrC, Clen, scalar);
            break;
        default:
            integerDoubleLoop<T, IntegerDoubleOperation::DoubleMinusInteger>(
                ptrA, ptrC, Clen, scalar);
            break;
        }
    }
    return res;
}
//=============================================================================
// integerKernel is the same-class saturated kernel (integer_addition<T>, ...):
// it is used unchanged when the scalar is an exact value of T, so the common
// case (x + 1) keeps its cost. Otherwise (fraction, NaN, Inf, out of range)
// the exact kernel applies.
//=============================================================================
template <class T>
static ArrayOf
integerDoubleArithmetic(NelsonType classDestination, const ArrayOf& integerArray,
    const ArrayOf& doubleScalar, bool integerIsLeft, IntegerDoubleOperation operation,
    ArrayOf (*integerKernel)(NelsonType, const ArrayOf&, const ArrayOf&))
{
    double scalar = static_cast<const double*>(doubleScalar.dataPointer())[0];
    if (isExactIntegerValueOf<T>(scalar)) {
        ArrayOf converted(doubleScalar);
        converted.promoteType(classDestination);
        return integerIsLeft ? integerKernel(classDestination, integerArray, converted)
                             : integerKernel(classDestination, converted, integerArray);
    }
    return integerDoubleScalarKernel<T>(classDestination, integerArray, scalar, operation);
}
//=============================================================================
static inline bool
isIntegerDoubleScalarPair(const ArrayOf& A, const ArrayOf& B, bool& integerIsLeft)
{
    if (A.isIntegerType() && B.isDoubleType(true) && B.isScalar() && !B.isSparse()) {
        integerIsLeft = true;
        return true;
    }
    if (B.isIntegerType() && A.isDoubleType(true) && A.isScalar() && !A.isSparse()) {
        integerIsLeft = false;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
