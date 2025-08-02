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
    }
    // signed <-- signed
    else if (std::numeric_limits<TOUT>::is_signed && std::numeric_limits<TIN>::is_signed) {
        if (positive_overflow_possible && (value > std::numeric_limits<TOUT>::max())) {
            return std::numeric_limits<TOUT>::max();
        } else if (negative_overflow_possible && (value < std::numeric_limits<TOUT>::lowest())) {
            return std::numeric_limits<TOUT>::min();
        }
    }
    return static_cast<TOUT>(value);
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
        bool negativeOverflow = a <= std::numeric_limits<T>::max() - b;
        bool positiveOverflow = a >= std::numeric_limits<T>::min() - b;
        if (b < 0 ? positiveOverflow : negativeOverflow) {
            return a + b;
        } else {
            if (positiveOverflow) {
                return std::numeric_limits<T>::max();
            } else {
                return std::numeric_limits<T>::min();
            }
        }
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
    bool negativeOverflow = a <= std::numeric_limits<T>::max() + b;
    bool positiveOverflow = a >= std::numeric_limits<T>::min() + b;
    if (b > 0 ? positiveOverflow : negativeOverflow) {
        return a - b;
    } else {
        if (positiveOverflow) {
            return std::numeric_limits<T>::max();
        } else {
            return std::numeric_limits<T>::min();
        }
    }
}
//=============================================================================
template <class T>
T
scalar_scalar_integer_divide(T a, T b)
{
    if (b == 0) {
        if (a > 0) {
            return std::numeric_limits<T>::max();
        } else {
            return std::numeric_limits<T>::min();
        }
    }
    if (a == std::numeric_limits<T>::min() && b == -1) {
        return std::numeric_limits<T>::max();
    }
    if (a == std::numeric_limits<T>::max() && b == -1) {
        return std::numeric_limits<T>::min();
    }
    return numeric_cast<long double, T>(roundl((long double)a / (long double)b));
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
    T aa = ((std::numeric_limits<T>::max() / b) * 2)
        + ((((std::numeric_limits<T>::max() % b) * 2) + 1) / b);
    if (a > aa) {
        if (aa > 0) {
            return std::numeric_limits<T>::max();
        }
        return std::numeric_limits<T>::min();
    }
    return (T)(a * b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
