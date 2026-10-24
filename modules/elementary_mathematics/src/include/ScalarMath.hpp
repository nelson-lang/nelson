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
#include <cmath>
#include <limits>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
inline T
NelsonScalarModuloFloorOrValue(T x)
{
    return std::isfinite(x) ? std::floor(x + static_cast<T>(0.5)) : x;
}
//=============================================================================
template <class T>
inline bool
NelsonScalarModuloIsFiniteIntegerValue(T x)
{
    return std::isfinite(x) && std::floor(x) == x;
}
//=============================================================================
template <class T>
inline T
NelsonScalarModuloFiniteIntegerValue(T x, T y)
{
    T res = std::fmod(x, y);
    if (res == 0) {
        // a zero result carries the sign of the divisor
        return std::copysign(static_cast<T>(0), y);
    }
    if ((res > 0) != (y > 0)) {
        res += y;
    }
    return res;
}
//=============================================================================
template <class T>
inline T
NelsonScalarModulo(T x, T y)
{
    if (std::isinf(y)) {
        // Expected behavior: mod(x, +/-Inf) == x when x == 0 or sign(x) == sign(y), else
        // +/-Inf (y)
        if (!std::isfinite(x)) {
            return static_cast<T>(std::nan(""));
        }
        if (x == 0) {
            return std::copysign(static_cast<T>(0), y);
        }
        return ((x > 0) == (y > 0)) ? x : y;
    }
    T res = x;
    if (y == 0) {
        // mod(x, 0) is x; a zero result carries the sign of the divisor
        return (x == 0) ? std::copysign(static_cast<T>(0), y) : x;
    }
    if (NelsonScalarModuloIsFiniteIntegerValue(x) && NelsonScalarModuloIsFiniteIntegerValue(y)) {
        return NelsonScalarModuloFiniteIntegerValue(x, y);
    }
    T q = x / y;
    T eps = std::numeric_limits<T>::epsilon();
    T roundedQ = NelsonScalarModuloFloorOrValue(q);
    if (NelsonScalarModuloFloorOrValue(y) != y && (std::abs((q - roundedQ) / roundedQ) < eps)) {
        res = 0;
    } else {
        T n = std::floor(q);
        T tmp = y * n;
        res = x - tmp;
    }
    if (res == 0) {
        res = std::copysign(static_cast<T>(0), y);
    } else if (x != y) {
        res = std::copysign(res, y);
    }
    return res;
}
//=============================================================================
template <class T>
inline T
NelsonScalarMinimum(T x, T y)
{
    T res = std::fmin(x, y);
    // std::fmin does not guarantee the sign of a +0/-0 tie (the result depends
    // on the platform and on inlining); min must return -0 when either operand
    // is negative zero.
    if (res == static_cast<T>(0) && (std::signbit(x) || std::signbit(y))) {
        res = -static_cast<T>(0);
    }
    return res;
}
//=============================================================================
template <class T>
inline T
NelsonScalarMaximum(T x, T y)
{
    T res = std::fmax(x, y);
    // std::fmax does not guarantee the sign of a +0/-0 tie; max must return +0
    // when either operand is positive zero.
    if (res == static_cast<T>(0) && (!std::signbit(x) || !std::signbit(y))) {
        res = static_cast<T>(0);
    }
    return res;
}
//=============================================================================
template <class T>
inline T
NelsonScalarRemainderFloorOrValue(T x)
{
    return std::isfinite(x) ? std::floor(x + static_cast<T>(0.5)) : x;
}
//=============================================================================
template <class T>
inline T
NelsonScalarRemainder(T x, T y)
{
    T res = x;
    if (std::isinf(y)) {
        // Expected behavior: rem(x, +/-Inf) == x for finite x, NaN otherwise
        return std::isfinite(x) ? x : static_cast<T>(std::nan(""));
    }
    if (y != 0) {
        T q = x / y;
        T eps = std::numeric_limits<T>::epsilon();
        T roundedQ = NelsonScalarRemainderFloorOrValue(q);
        if (NelsonScalarRemainderFloorOrValue(y) != y
            && (std::abs((q - roundedQ) / roundedQ) < eps)) {
            res = 0;
        } else {
            T n = std::trunc(q);
            T tmp = y * n;
            res = x - tmp;
        }
    } else {
        res = std::nan("");
    }
    if (res == 0) {
        // a zero remainder carries the sign of the dividend
        res = std::copysign(static_cast<T>(0), x);
    }
    return res;
}
//=============================================================================
inline double
NelsonScalarGamma(double v)
{
    if ((v == 0) || std::isinf(v) || (v >= 172)) {
        return std::numeric_limits<double>::infinity();
    }
    if ((v <= 0) && (std::floor(v) == v)) {
        return std::numeric_limits<double>::infinity();
    }
    return std::tgamma(v);
}
//=============================================================================
inline float
NelsonScalarGamma(float v)
{
    if ((v == 0) || std::isinf(v) || (v >= 36)) {
        return std::numeric_limits<float>::infinity();
    }
    if ((v <= 0) && (std::floor(v) == v)) {
        return std::numeric_limits<float>::infinity();
    }
    return std::tgamma(v);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
