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
#if _MSC_VER
#pragma warning(disable : 4146)
#endif
//=============================================================================
#include <type_traits>
#include <limits>
#include <cmath>
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
mustCastIntegerAsLongDouble(NelsonType variableClass)
{
    return IS_INTEGER_TYPE(variableClass)
        && ((variableClass == NLS_UINT64) || (variableClass == NLS_INT64));
}
//=============================================================================
static bool
mustCastIntegerAsDouble(NelsonType variableClass)
{
    return (IS_INTEGER_TYPE(variableClass)
        && (variableClass == NLS_UINT32 || variableClass == NLS_INT32));
}
//=============================================================================
template <typename TIN, typename TOUT>
inline TOUT
numeric_cast(TIN value)
{
    if constexpr (std::is_floating_point<TIN>::value && std::is_integral<TOUT>::value) {
        // Use power-of-two bounds (2^digits and -(2^digits)): they are exact in floating
        // point, while TOUT max may not be (int64 max rounds up to 2^63 as a double).
        if (std::isnan(static_cast<long double>(value))) {
            return static_cast<TOUT>(0);
        }
        const long double v = static_cast<long double>(value);
        const long double upperBound = std::ldexp(1.0L, std::numeric_limits<TOUT>::digits);
        if (v >= upperBound) {
            return std::numeric_limits<TOUT>::max();
        }
        if constexpr (std::numeric_limits<TOUT>::is_signed) {
            if (v < -upperBound) {
                return std::numeric_limits<TOUT>::min();
            }
        } else {
            if (v <= 0.0L) {
                return static_cast<TOUT>(0);
            }
        }
        return static_cast<TOUT>(value);
    } else {
        const bool positive_overflow_possible
            = std::numeric_limits<TOUT>::max() < std::numeric_limits<TIN>::max();
        const bool negative_overflow_possible = std::numeric_limits<TIN>::is_signed
            || (std::numeric_limits<TOUT>::lowest() > std::numeric_limits<TIN>::lowest());

        // unsigned <-- unsigned
        if ((!std::numeric_limits<TOUT>::is_signed) && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        }
        // unsigned <-- signed
        else if ((!std::numeric_limits<TOUT>::is_signed) && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            } else if (negative_overflow_possible && (value < 0)) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        // signed <-- unsigned
        else if (std::numeric_limits<TOUT>::is_signed && (!std::numeric_limits<TIN>::is_signed)) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            }
        } else if (std::numeric_limits<TOUT>::is_signed && std::numeric_limits<TIN>::is_signed) {
            if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
                return std::numeric_limits<TOUT>::max();
            } else if (negative_overflow_possible
                && (value < std::numeric_limits<TOUT>::lowest())) {
                return std::numeric_limits<TOUT>::min();
            }
        }
        return static_cast<TOUT>(value);
    }
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_bitand(T a, T b)
{
    return T(a & b);
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_bitor(T a, T b)
{
    return T(a | b);
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_bitxor(T a, T b)
{
    return T(a ^ b);
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_addition(T a, T b)
{
    if (std::is_signed<T>()) {
        // Compute each bound only in the branch where the subtraction cannot overflow.
        if (b >= 0) {
            if (a > std::numeric_limits<T>::max() - b) {
                return std::numeric_limits<T>::max();
            }
        } else {
            if (a < std::numeric_limits<T>::min() - b) {
                return std::numeric_limits<T>::min();
            }
        }
        return a + b;
    } else {
        T op = a + b;
        if ((op < a) || (op < b)) {
            return std::numeric_limits<T>::max();
        } else {
            return (T)op;
        }
    }
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_subtraction(T a, T b)
{
    if (std::is_signed<T>()) {
        // Compute each bound only where the addition cannot overflow.
        if (b <= 0) {
            if (a > std::numeric_limits<T>::max() + b) {
                return std::numeric_limits<T>::max();
            }
        } else {
            if (a < std::numeric_limits<T>::min() + b) {
                return std::numeric_limits<T>::min();
            }
        }
        return a - b;
    }
    // unsigned: saturate to 0 when a < b.
    if (a < b) {
        return std::numeric_limits<T>::min();
    }
    return a - b;
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_divide(T a, T b)
{
    if (b == 0) {
        if (a == 0) {
            return T(0);
        }
        if (a > 0) {
            return std::numeric_limits<T>::max();
        }
        return std::numeric_limits<T>::min();
    }
    if (std::is_signed<T>() && a == std::numeric_limits<T>::min() && b == T(-1)) {
        // -intmin is not representable: saturate to intmax.
        return std::numeric_limits<T>::max();
    }
    // Round half away from zero in integer arithmetic: int64/uint64 values above 2^53
    // are not exact as double (long double == double with MSVC).
    using UT = typename std::make_unsigned<T>::type;
    T q = static_cast<T>(a / b);
    T r = static_cast<T>(a % b);
    if (r != 0) {
        // |r| and |b| are computed in the unsigned type to avoid overflow on abs(intmin).
        UT absR = static_cast<UT>(r);
        UT absB = static_cast<UT>(b);
        if (std::is_signed<T>()) {
            if (r < 0) {
                absR = static_cast<UT>(0) - absR;
            }
            if (b < 0) {
                absB = static_cast<UT>(0) - absB;
            }
        }
        // 2 * |r| >= |b|, written without overflow (|r| < |b| always holds).
        if (absR >= absB - absR) {
            bool negativeQuotient = std::is_signed<T>() && ((a < 0) != (b < 0));
            q = negativeQuotient ? static_cast<T>(q - 1) : static_cast<T>(q + 1);
        }
    }
    return q;
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_times(T a, T b)
{
    if (std::is_signed<T>()) {
        if (a > 0 && b > 0 && a > std::numeric_limits<T>::max() / b) {
            return std::numeric_limits<T>::max();
        }
        if (a < 0 && b > 0 && a < std::numeric_limits<T>::min() / b) {
            return std::numeric_limits<T>::min();
        }
        if (a > 0 && b < 0 && b < std::numeric_limits<T>::min() / a) {
            return std::numeric_limits<T>::min();
        }
        if (a < 0 && b < 0
            && (a <= std::numeric_limits<T>::min() || b <= std::numeric_limits<T>::min()
                || -a > std::numeric_limits<T>::max() / -b)) {
            return std::numeric_limits<T>::max();
        }
        return (T)(a * b);
    }
    if ((a == 0) || (b == 0)) {
        return (T)(0);
    }
    if ((a == 1) || (b == 1)) {
        if (a == 1) {
            return (T)b;
        }
        return (T)a;
    }
    if (a > std::numeric_limits<T>::max() / b) {
        return std::numeric_limits<T>::max();
    }
    return (T)(a * b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
