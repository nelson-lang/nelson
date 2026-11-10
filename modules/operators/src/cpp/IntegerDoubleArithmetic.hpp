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
#include <vector>
#if defined(_MSC_VER)
#include <intrin.h>
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#include "IntegerOperations.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// integer array with a real double scalar: rounded half away from zero, saturated, NaN gives 0;
// 64-bit classes are exact, smaller classes are computed in double
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
        // below 2^65 an integer of the other sign can bring the sum back in range
        const bool otherSign = integerPart.magnitude != 0 && integerPart.negative != (whole < 0);
        if (otherSign && std::fabs(whole) < 36893488147419103232.0) {
            const uint64 excess = static_cast<uint64>(std::fabs(whole) - 18446744073709551616.0);
            if (integerPart.magnitude > excess) {
                // 2^64 + excess - magnitude, which is below 2^64 here
                SignedMagnitude64 sum
                    = { whole < 0, excess + (static_cast<uint64>(0) - integerPart.magnitude) };
                return saturateSignedMagnitude<T>(sum);
            }
        }
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
// classes of 32 bits or less: (+-a) + d is computed in double, as the reference does
// (int32(3) + 0.49999999999999994 is 4), then rounded half away from zero and saturated
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
    const double sign = (operation == IntegerDoubleOperation::DoubleMinusInteger) ? -1.0 : 1.0;
    const double d = (operation == IntegerDoubleOperation::IntegerMinusDouble) ? -scalar : scalar;
    // clamping before rounding gives the same result, and the value then fits 32 bits
    const double lowest = static_cast<double>(std::numeric_limits<T>::min());
    const double highest = static_cast<double>(std::numeric_limits<T>::max());
    using Wide = typename std::conditional<std::is_same<T, uint32>::value, int64, int32>::type;
    auto valueOf = [&](T a) -> T {
        const double v = std::min(highest, std::max(lowest, sign * static_cast<double>(a) + d));
        Wide n = static_cast<Wide>(v);
        const double fraction = v - static_cast<double>(n);
        n += static_cast<Wide>(fraction >= 0.5) - static_cast<Wide>(fraction <= -0.5);
        return static_cast<T>(n);
    };
    if constexpr (sizeof(T) <= 2) {
        // 8 and 16 bit classes: a table of every input value, when the array is large enough
        constexpr indexType tableSize = static_cast<indexType>(1) << (8 * sizeof(T));
        if (Clen >= tableSize / 4) {
            using U = typename std::make_unsigned<T>::type;
            std::vector<T> table(tableSize);
            for (indexType u = 0; u < tableSize; ++u) {
                table[u] = valueOf(static_cast<T>(static_cast<U>(u)));
            }
            const T* lookup = table.data();
            OMP_PARALLEL_FOR_LOOP(Clen)
            for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
                ptrC[k] = lookup[static_cast<U>(ptrA[k])];
            }
            return;
        }
    }
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        ptrC[k] = valueOf(ptrA[k]);
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
// int64/uint64 with a double scalar by .*, ./ and .\: the exact value is rounded half away
// from zero and saturated, never through a double that would round values above 2^53.
//=============================================================================
enum class Integer64DoubleOperation
{
    Times,
    IntegerOverDouble,
    DoubleOverInteger
};
//=============================================================================
struct UInt128Parts
{
    uint64 hi;
    uint64 lo;
};
//=============================================================================
static inline UInt128Parts
multiplyUInt64(uint64 a, uint64 b)
{
#if defined(_MSC_VER) && defined(_M_X64)
    uint64 hi = 0;
    uint64 lo = _umul128(a, b, &hi);
    return { hi, lo };
#elif defined(_MSC_VER) && defined(_M_ARM64)
    return { __umulh(a, b), a * b };
#elif defined(__SIZEOF_INT128__)
    unsigned __int128 p = static_cast<unsigned __int128>(a) * b;
    return { static_cast<uint64>(p >> 64), static_cast<uint64>(p) };
#else
    const uint64 aLo = a & 0xFFFFFFFFULL;
    const uint64 aHi = a >> 32;
    const uint64 bLo = b & 0xFFFFFFFFULL;
    const uint64 bHi = b >> 32;
    const uint64 ll = aLo * bLo;
    const uint64 lh = aLo * bHi;
    const uint64 hl = aHi * bLo;
    const uint64 hh = aHi * bHi;
    const uint64 middle = (ll >> 32) + (lh & 0xFFFFFFFFULL) + (hl & 0xFFFFFFFFULL);
    return { hh + (lh >> 32) + (hl >> 32) + (middle >> 32), (middle << 32) | (ll & 0xFFFFFFFFULL) };
#endif
}
//=============================================================================
// (hi:lo) / divisor with hi < divisor, so the quotient fits in 64 bits
static inline uint64
divideUInt128(uint64 hi, uint64 lo, uint64 divisor, uint64& remainder)
{
#if defined(_MSC_VER) && defined(_M_X64) && (_MSC_VER >= 1920)
    return _udiv128(hi, lo, divisor, &remainder);
#elif defined(__SIZEOF_INT128__)
    unsigned __int128 n = (static_cast<unsigned __int128>(hi) << 64) | lo;
    remainder = static_cast<uint64>(n % divisor);
    return static_cast<uint64>(n / divisor);
#else
    uint64 quotient = 0;
    for (int bit = 63; bit >= 0; --bit) {
        const bool carry = (hi >> 63) != 0;
        hi = (hi << 1) | (lo >> 63);
        lo <<= 1;
        if (carry || hi >= divisor) {
            hi -= divisor;
            quotient |= (static_cast<uint64>(1) << bit);
        }
    }
    remainder = hi;
    return quotient;
#endif
}
//=============================================================================
static inline int
bitLengthUInt64(uint64 value)
{
#if defined(_MSC_VER) && (defined(_M_X64) || defined(_M_ARM64))
    unsigned long index = 0;
    return _BitScanReverse64(&index, value) ? static_cast<int>(index) + 1 : 0;
#elif defined(__GNUC__) || defined(__clang__)
    return value == 0 ? 0 : 64 - __builtin_clzll(value);
#else
    int length = 0;
    while (value != 0) {
        value >>= 1;
        ++length;
    }
    return length;
#endif
}
//=============================================================================
// value * 2^shift as 128 bits, the caller ensures nothing is lost (shift < 128)
static inline UInt128Parts
shiftLeftUInt64(uint64 value, int shift)
{
    if (shift == 0) {
        return { 0, value };
    }
    if (shift < 64) {
        return { value >> (64 - shift), value << shift };
    }
    return { value << (shift - 64), 0 };
}
//=============================================================================
// magnitude rounded up when the remainder is at least half the divisor
static inline uint64
roundQuotientMagnitude(uint64 quotient, uint64 remainder, uint64 divisor)
{
    if (remainder != 0 && remainder >= divisor - remainder) {
        return (quotient == std::numeric_limits<uint64>::max()) ? quotient : quotient + 1;
    }
    return quotient;
}
//=============================================================================
// |d| = mantissa * 2^exponent with an odd mantissa (d finite and not zero)
struct DoubleParts64
{
    uint64 mantissa = 0;
    int exponent = 0;
    int mantissaBits = 0;
};
//=============================================================================
static inline DoubleParts64
decomposeDouble64(double d)
{
    int binaryExponent = 0;
    double fraction = std::frexp(std::fabs(d), &binaryExponent);
    DoubleParts64 parts;
    parts.mantissa = static_cast<uint64>(std::ldexp(fraction, 53));
    parts.exponent = binaryExponent - 53;
    while ((parts.mantissa & 1) == 0) {
        parts.mantissa >>= 1;
        parts.exponent++;
    }
    parts.mantissaBits = bitLengthUInt64(parts.mantissa);
    return parts;
}
//=============================================================================
// the magnitudes below return the uint64 maximum when the exact value is larger
// round(a * mantissa * 2^exponent)
static inline uint64
productMagnitude64(uint64 a, const DoubleParts64& d)
{
    UInt128Parts p = multiplyUInt64(a, d.mantissa);
    if (d.exponent >= 0) {
        const int e = d.exponent;
        if (p.hi != 0 || e >= 64 || (e > 0 && (p.lo >> (64 - e)) != 0)) {
            return std::numeric_limits<uint64>::max();
        }
        return p.lo << e;
    }
    const int k = -d.exponent;
    if (k >= 128) {
        return 0;
    }
    uint64 quotientHi = 0;
    uint64 quotientLo = 0;
    uint64 roundBit = 0;
    if (k < 64) {
        quotientLo = (p.lo >> k) | (p.hi << (64 - k));
        quotientHi = p.hi >> k;
        roundBit = (p.lo >> (k - 1)) & 1;
    } else if (k == 64) {
        quotientLo = p.hi;
        roundBit = p.lo >> 63;
    } else {
        quotientLo = p.hi >> (k - 64);
        roundBit = (p.hi >> (k - 65)) & 1;
    }
    if (quotientHi != 0) {
        return std::numeric_limits<uint64>::max();
    }
    if (roundBit != 0) {
        return (quotientLo == std::numeric_limits<uint64>::max()) ? quotientLo : quotientLo + 1;
    }
    return quotientLo;
}
//=============================================================================
// round(a / (mantissa * 2^exponent)), a > 0
static inline uint64
integerOverDoubleMagnitude64(uint64 a, const DoubleParts64& d)
{
    if (d.exponent >= 0) {
        const int divisorBits = d.mantissaBits + d.exponent;
        if (divisorBits <= 64) {
            const uint64 divisor = d.mantissa << d.exponent;
            return roundQuotientMagnitude(a / divisor, a % divisor, divisor);
        }
        // the divisor is at least 2^64 > a: the quotient is 1 only when 2a >= divisor
        if (divisorBits > 65) {
            return 0;
        }
        UInt128Parts divisor = shiftLeftUInt64(d.mantissa, d.exponent);
        UInt128Parts twice = { a >> 63, a << 1 };
        const bool atLeastHalf
            = (twice.hi > divisor.hi) || (twice.hi == divisor.hi && twice.lo >= divisor.lo);
        return atLeastHalf ? 1 : 0;
    }
    const int k = -d.exponent;
    if (bitLengthUInt64(a) + k - d.mantissaBits > 64) {
        return std::numeric_limits<uint64>::max();
    }
    UInt128Parts numerator = shiftLeftUInt64(a, k);
    if (numerator.hi >= d.mantissa) {
        return std::numeric_limits<uint64>::max();
    }
    uint64 remainder = 0;
    uint64 quotient = divideUInt128(numerator.hi, numerator.lo, d.mantissa, remainder);
    return roundQuotientMagnitude(quotient, remainder, d.mantissa);
}
//=============================================================================
// round(mantissa * 2^exponent / a), a > 0
static inline uint64
doubleOverIntegerMagnitude64(const DoubleParts64& d, uint64 a)
{
    if (d.exponent >= 0) {
        if (d.mantissaBits + d.exponent - bitLengthUInt64(a) > 64) {
            return std::numeric_limits<uint64>::max();
        }
        UInt128Parts numerator = shiftLeftUInt64(d.mantissa, d.exponent);
        if (numerator.hi >= a) {
            return std::numeric_limits<uint64>::max();
        }
        uint64 remainder = 0;
        uint64 quotient = divideUInt128(numerator.hi, numerator.lo, a, remainder);
        return roundQuotientMagnitude(quotient, remainder, a);
    }
    const int k = -d.exponent;
    // a divisor of 2^64 or more is above twice the 53-bit mantissa: the quotient rounds to 0
    if (bitLengthUInt64(a) + k > 64) {
        return 0;
    }
    const uint64 divisor = a << k;
    return roundQuotientMagnitude(d.mantissa / divisor, d.mantissa % divisor, divisor);
}
//=============================================================================
template <class T, Integer64DoubleOperation operation>
static T
exactInteger64DoubleValue(T x, double d, const DoubleParts64& parts)
{
    if (std::isnan(d)) {
        return static_cast<T>(0);
    }
    const SignedMagnitude64 a = signedMagnitudeOf(x);
    const bool negative = a.negative != std::signbit(d);
    const uint64 saturated = std::numeric_limits<uint64>::max();
    uint64 magnitude = 0;
    if constexpr (operation == Integer64DoubleOperation::Times) {
        if (a.magnitude == 0 || d == 0) {
            return static_cast<T>(0);
        }
        magnitude = std::isinf(d) ? saturated : productMagnitude64(a.magnitude, parts);
    } else if constexpr (operation == Integer64DoubleOperation::IntegerOverDouble) {
        if (a.magnitude == 0 || std::isinf(d)) {
            return static_cast<T>(0);
        }
        magnitude = (d == 0) ? saturated : integerOverDoubleMagnitude64(a.magnitude, parts);
    } else {
        if (d == 0) {
            return static_cast<T>(0);
        }
        if (a.magnitude == 0) {
            // d / 0 is an infinity with the sign of d
            return saturateSignedMagnitude<T>({ std::signbit(d), saturated });
        }
        magnitude = std::isinf(d) ? saturated : doubleOverIntegerMagnitude64(parts, a.magnitude);
    }
    return saturateSignedMagnitude<T>({ negative, magnitude });
}
//=============================================================================
// a * b - p exactly, for p = a * b rounded (no overflow or underflow)
static inline double
productRoundingError(double a, double b, double p)
{
#if defined(FP_FAST_FMA) || defined(__FMA__) || defined(__ARM_FEATURE_FMA)
    return std::fma(a, b, -p);
#else
    const double splitter = 134217729.0;
    const double ca = splitter * a;
    const double aHi = ca - (ca - a);
    const double aLo = a - aHi;
    const double cb = splitter * b;
    const double bHi = cb - (cb - b);
    const double bLo = b - bHi;
    return ((aHi * bHi - p) + aHi * bLo + aLo * bHi) + aLo * bLo;
#endif
}
//=============================================================================
// sign of the exact quotient minus q (q = value / divisor rounded)
static inline double
quotientRoundingDirection(double value, double divisor, double q)
{
    const double p = q * divisor;
    return ((value - p) - productRoundingError(q, divisor, p)) * divisor;
}
//=============================================================================
// sign of the exact result minus its rounded double q
template <Integer64DoubleOperation operation>
static inline double
roundingDirection(double value, double d, double q)
{
    if constexpr (operation == Integer64DoubleOperation::Times) {
        return productRoundingError(value, d, q);
    } else if constexpr (operation == Integer64DoubleOperation::IntegerOverDouble) {
        return quotientRoundingDirection(value, d, q);
    } else {
        return quotientRoundingDirection(d, value, q);
    }
}
//=============================================================================
// below 2^53 the double result q is used: a K + 0.5 tie is moved to the side of
// the exact value, given by the exact rounding error of q
template <class T, Integer64DoubleOperation operation>
static void
integer64DoubleLoop(const T* ptrA, T* ptrC, indexType Clen, double d)
{
    constexpr double limit53 = 9007199254740992.0;
    constexpr double limit52 = 4503599627370496.0;
    DoubleParts64 parts;
    if (std::isfinite(d) && d != 0) {
        parts = decomposeDouble64(d);
    }
    // an infinite or zero double, or a power of two divisor, never leaves a tie to settle
    const bool withDirection = parts.mantissa != 0
        && !(operation == Integer64DoubleOperation::IntegerOverDouble && parts.mantissa == 1);
    OMP_PARALLEL_FOR_LOOP(Clen)
    for (ompIndexType k = 0; k < (ompIndexType)Clen; ++k) {
        const T x = ptrA[k];
        const double value = static_cast<double>(x);
        if (std::fabs(value) <= limit53) {
            double q;
            if constexpr (operation == Integer64DoubleOperation::Times) {
                q = value * d;
            } else if constexpr (operation == Integer64DoubleOperation::IntegerOverDouble) {
                q = value / d;
            } else {
                q = d / value;
            }
            if (std::fabs(q) < limit52) {
                double rounded = std::round(q);
                // no data dependent branch: a tie moves by half towards the exact value
                const double direction
                    = withDirection ? roundingDirection<operation>(value, d, q) : 0.0;
                const double tie = static_cast<double>(std::fabs(rounded - q) == 0.5);
                const double side
                    = static_cast<double>(direction > 0) - static_cast<double>(direction < 0);
                rounded += tie * side * side * ((q + 0.5 * side) - rounded);
                if constexpr (std::is_signed<T>::value) {
                    ptrC[k] = static_cast<T>(rounded);
                } else {
                    ptrC[k] = (rounded < 0) ? static_cast<T>(0) : static_cast<T>(rounded);
                }
                continue;
            }
        }
        ptrC[k] = exactInteger64DoubleValue<T, operation>(x, d, parts);
    }
}
//=============================================================================
template <class T>
static ArrayOf
integer64DoubleScalarKernel(NelsonType classDestination, const ArrayOf& integerArray, double d,
    Integer64DoubleOperation operation)
{
    Dimensions dimsC = integerArray.dimensions();
    indexType Clen = dimsC.elementCount();
    T* ptrC = (T*)ArrayOf::allocateArrayOf(classDestination, Clen);
    ArrayOf res(classDestination, dimsC, ptrC, false);
    const T* ptrA = (const T*)integerArray.dataPointer();
    switch (operation) {
    case Integer64DoubleOperation::Times:
        integer64DoubleLoop<T, Integer64DoubleOperation::Times>(ptrA, ptrC, Clen, d);
        break;
    case Integer64DoubleOperation::IntegerOverDouble:
        integer64DoubleLoop<T, Integer64DoubleOperation::IntegerOverDouble>(ptrA, ptrC, Clen, d);
        break;
    default:
        integer64DoubleLoop<T, Integer64DoubleOperation::DoubleOverInteger>(ptrA, ptrC, Clen, d);
        break;
    }
    return res;
}
//=============================================================================
// true (and res set) for an int64/uint64 array with a real double scalar
static inline bool
tryInteger64DoubleScalar(const ArrayOf& integerArray, const ArrayOf& doubleScalar,
    Integer64DoubleOperation operation, ArrayOf& res)
{
    const NelsonType integerClass = integerArray.dataClass();
    if ((integerClass != NLS_INT64 && integerClass != NLS_UINT64) || integerArray.isSparse()
        || doubleScalar.dataClass() != NLS_DOUBLE || !doubleScalar.isScalar()
        || doubleScalar.isSparse()) {
        return false;
    }
    const double d = static_cast<const double*>(doubleScalar.dataPointer())[0];
    if (integerClass == NLS_INT64) {
        res = integer64DoubleScalarKernel<int64>(NLS_INT64, integerArray, d, operation);
    } else {
        res = integer64DoubleScalarKernel<uint64>(NLS_UINT64, integerArray, d, operation);
    }
    return true;
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
