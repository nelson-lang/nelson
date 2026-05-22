//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LinearInterpolation1D.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <cmath>
#include <type_traits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class Interp1DMethod
{
    LINEAR,
    NEAREST,
    PREVIOUS,
    NEXT,
    HERMITE
};
//=============================================================================
enum class ExtrapolationMode
{
    DEFAULT_NAN,
    EXTRAPOLATE,
    CONSTANT
};
//=============================================================================
static Interp1DMethod
parseMethodName(const std::wstring& methodName)
{
    if (methodName == L"linear" || methodName.empty()) {
        return Interp1DMethod::LINEAR;
    }
    if (methodName == L"nearest") {
        return Interp1DMethod::NEAREST;
    }
    if (methodName == L"previous") {
        return Interp1DMethod::PREVIOUS;
    }
    if (methodName == L"next") {
        return Interp1DMethod::NEXT;
    }
    if (methodName == L"pchip" || methodName == L"cubic" || methodName == L"makima"
        || methodName == L"spline") {
        return Interp1DMethod::HERMITE;
    }
    Error(_W("Interpolation method not supported by the native 1-D engine."));
    return Interp1DMethod::LINEAR;
}
//=============================================================================
static ExtrapolationMode
parseExtrapolationMode(const std::wstring& extrapolationMode)
{
    if (extrapolationMode == L"extrap") {
        return ExtrapolationMode::EXTRAPOLATE;
    }
    if (extrapolationMode == L"constant") {
        return ExtrapolationMode::CONSTANT;
    }
    return ExtrapolationMode::DEFAULT_NAN;
}
//=============================================================================
template <class T>
static bool
isStrictlyIncreasing(const T* x, indexType n)
{
    for (indexType k = 1; k < n; k++) {
        if (!(x[k] > x[k - 1])) {
            return false;
        }
    }
    return true;
}
//=============================================================================
template <class T>
static indexType
findLeftIndex(const T* x, indexType n, T q)
{
    if (q <= x[0]) {
        return 0;
    }
    if (q >= x[n - 1]) {
        return n - 2;
    }
    const T* it = std::upper_bound(x, x + n, q);
    indexType idx = static_cast<indexType>(it - x) - 1;
    if (idx >= n - 1) {
        idx = n - 2;
    }
    return idx;
}
//=============================================================================
template <class T>
static T
numericNaN()
{
    return static_cast<T>(std::nan(""));
}
//=============================================================================
template <class T>
static bool
sameSign(T a, T b)
{
    return (a > static_cast<T>(0) && b > static_cast<T>(0))
        || (a < static_cast<T>(0) && b < static_cast<T>(0));
}
//=============================================================================
template <class T>
static T
pchipEndpointSlope(T h1, T h2, T del1, T del2)
{
    T d = ((static_cast<T>(2) * h1 + h2) * del1 - h1 * del2) / (h1 + h2);
    if (!sameSign(d, del1)) {
        return static_cast<T>(0);
    }
    if (!sameSign(del1, del2) && std::abs(d) > std::abs(static_cast<T>(3) * del1)) {
        return static_cast<T>(3) * del1;
    }
    return d;
}
//=============================================================================
template <class T>
static std::vector<T>
pchipSlopesReal(const T* x, const T* y, indexType n)
{
    std::vector<T> d(n, static_cast<T>(0));
    std::vector<T> h(n - 1);
    std::vector<T> delta(n - 1);
    for (indexType k = 0; k < n - 1; k++) {
        h[k] = x[k + 1] - x[k];
        delta[k] = (y[k + 1] - y[k]) / h[k];
    }
    if (n == 2) {
        d[0] = delta[0];
        d[1] = delta[0];
        return d;
    }
    for (indexType k = 1; k < n - 1; k++) {
        if (sameSign(delta[k - 1], delta[k])) {
            T w1 = static_cast<T>(2) * h[k] + h[k - 1];
            T w2 = h[k] + static_cast<T>(2) * h[k - 1];
            d[k] = (w1 + w2) / (w1 / delta[k - 1] + w2 / delta[k]);
        } else {
            d[k] = static_cast<T>(0);
        }
    }
    d[0] = pchipEndpointSlope(h[0], h[1], delta[0], delta[1]);
    d[n - 1] = pchipEndpointSlope(h[n - 2], h[n - 3], delta[n - 2], delta[n - 3]);
    return d;
}
//=============================================================================
template <class T>
static void
computeHermiteBasis(T x0, T x1, T q, T& h00, T& h10, T& h01, T& h11, T& h)
{
    h = x1 - x0;
    T t = (q - x0) / h;
    T t2 = t * t;
    T t3 = t2 * t;
    h00 = static_cast<T>(2) * t3 - static_cast<T>(3) * t2 + static_cast<T>(1);
    h10 = t3 - static_cast<T>(2) * t2 + t;
    h01 = -static_cast<T>(2) * t3 + static_cast<T>(3) * t2;
    h11 = t3 - t2;
}
//=============================================================================
template <class T>
static T
getRealScalarOrNaN(const ArrayOf* value)
{
    if (value == nullptr || value->isEmpty()) {
        return numericNaN<T>();
    }
    ArrayOf scalar(*value);
    if (scalar.getElementCount() != 1) {
        Error(_W("Scalar extrapolation value expected."));
    }
    if (std::is_same<T, single>::value) {
        scalar.promoteType(NLS_SINGLE);
        return static_cast<T>(scalar.getContentAsSingleScalar());
    }
    scalar.promoteType(NLS_DOUBLE);
    return static_cast<T>(scalar.getContentAsDoubleScalar());
}
//=============================================================================
template <class T>
static void
interpolateReal(const T* x, const T* y, indexType n, const T* q, indexType nq, T* out,
    Interp1DMethod method, ExtrapolationMode extrapolationMode, T extrapolationValue)
{
    std::vector<T> slopes;
    if (method == Interp1DMethod::HERMITE) {
        slopes = pchipSlopesReal(x, y, n);
    }
    OMP_PARALLEL_FOR_LOOP(nq)
    for (ompIndexType k = 0; k < (ompIndexType)nq; k++) {
        T qk = q[k];
        if (std::isnan((double)qk)) {
            out[k] = numericNaN<T>();
            continue;
        }
        bool outside = qk < x[0] || qk > x[n - 1];
        if (outside && extrapolationMode == ExtrapolationMode::DEFAULT_NAN) {
            out[k] = numericNaN<T>();
            continue;
        }
        if (outside && extrapolationMode == ExtrapolationMode::CONSTANT) {
            out[k] = extrapolationValue;
            continue;
        }

        indexType left = findLeftIndex(x, n, qk);
        switch (method) {
        case Interp1DMethod::NEAREST: {
            T dl = std::abs(qk - x[left]);
            T dr = std::abs(x[left + 1] - qk);
            out[k] = (dl <= dr) ? y[left] : y[left + 1];
        } break;
        case Interp1DMethod::PREVIOUS: {
            if (qk <= x[0]) {
                out[k] = y[0];
            } else if (qk >= x[n - 1]) {
                out[k] = y[n - 1];
            } else {
                out[k] = y[left];
            }
        } break;
        case Interp1DMethod::NEXT: {
            if (qk <= x[0]) {
                out[k] = y[0];
            } else if (qk >= x[n - 1]) {
                out[k] = y[n - 1];
            } else if (qk == x[left]) {
                out[k] = y[left];
            } else {
                out[k] = y[left + 1];
            }
        } break;
        case Interp1DMethod::HERMITE: {
            T h00, h10, h01, h11, h;
            computeHermiteBasis(x[left], x[left + 1], qk, h00, h10, h01, h11, h);
            out[k] = h00 * y[left] + h10 * h * slopes[left] + h01 * y[left + 1]
                + h11 * h * slopes[left + 1];
        } break;
        default: {
            T frac = (qk - x[left]) / (x[left + 1] - x[left]);
            out[k] = y[left] + frac * (y[left + 1] - y[left]);
        } break;
        }
    }
}
//=============================================================================
template <class T>
static void
interpolateComplex(const T* x, const T* y, indexType n, const T* q, indexType nq, T* out,
    Interp1DMethod method, ExtrapolationMode extrapolationMode, T extrapolationReal,
    T extrapolationImag)
{
    std::vector<T> slopesReal;
    std::vector<T> slopesImag;
    if (method == Interp1DMethod::HERMITE) {
        std::vector<T> realValues(n);
        std::vector<T> imagValues(n);
        for (indexType k = 0; k < n; k++) {
            realValues[k] = y[2 * k];
            imagValues[k] = y[2 * k + 1];
        }
        slopesReal = pchipSlopesReal(x, realValues.data(), n);
        slopesImag = pchipSlopesReal(x, imagValues.data(), n);
    }
    OMP_PARALLEL_FOR_LOOP(nq)
    for (ompIndexType k = 0; k < (ompIndexType)nq; k++) {
        T qk = q[k];
        if (std::isnan((double)qk)) {
            out[2 * k] = numericNaN<T>();
            out[2 * k + 1] = numericNaN<T>();
            continue;
        }
        bool outside = qk < x[0] || qk > x[n - 1];
        if (outside && extrapolationMode == ExtrapolationMode::DEFAULT_NAN) {
            out[2 * k] = numericNaN<T>();
            out[2 * k + 1] = numericNaN<T>();
            continue;
        }
        if (outside && extrapolationMode == ExtrapolationMode::CONSTANT) {
            out[2 * k] = extrapolationReal;
            out[2 * k + 1] = extrapolationImag;
            continue;
        }

        indexType left = findLeftIndex(x, n, qk);
        indexType valueIndex = left;
        if (method == Interp1DMethod::NEAREST) {
            T dl = std::abs(qk - x[left]);
            T dr = std::abs(x[left + 1] - qk);
            valueIndex = (dl <= dr) ? left : left + 1;
        } else if (method == Interp1DMethod::NEXT && qk != x[left] && qk < x[n - 1]) {
            valueIndex = left + 1;
        } else if (method == Interp1DMethod::NEXT && qk >= x[n - 1]) {
            valueIndex = n - 1;
        } else if (method == Interp1DMethod::PREVIOUS && qk >= x[n - 1]) {
            valueIndex = n - 1;
        }

        if (method == Interp1DMethod::HERMITE) {
            T h00, h10, h01, h11, h;
            computeHermiteBasis(x[left], x[left + 1], qk, h00, h10, h01, h11, h);
            out[2 * k] = h00 * y[2 * left] + h10 * h * slopesReal[left] + h01 * y[2 * (left + 1)]
                + h11 * h * slopesReal[left + 1];
            out[2 * k + 1] = h00 * y[2 * left + 1] + h10 * h * slopesImag[left]
                + h01 * y[2 * (left + 1) + 1] + h11 * h * slopesImag[left + 1];
        } else if (method == Interp1DMethod::LINEAR) {
            T frac = (qk - x[left]) / (x[left + 1] - x[left]);
            out[2 * k] = y[2 * left] + frac * (y[2 * (left + 1)] - y[2 * left]);
            out[2 * k + 1] = y[2 * left + 1] + frac * (y[2 * (left + 1) + 1] - y[2 * left + 1]);
        } else {
            out[2 * k] = y[2 * valueIndex];
            out[2 * k + 1] = y[2 * valueIndex + 1];
        }
    }
}
//=============================================================================
template <class T>
static T
getComplexScalarPartOrNaN(const ArrayOf* value, bool imaginary)
{
    if (value == nullptr || value->isEmpty()) {
        return numericNaN<T>();
    }
    ArrayOf scalar(*value);
    if (scalar.getElementCount() != 1) {
        Error(_W("Scalar extrapolation value expected."));
    }
    if (!scalar.isComplex()) {
        return imaginary ? static_cast<T>(0) : getRealScalarOrNaN<T>(value);
    }
    if (scalar.getDataClass() == NLS_SCOMPLEX) {
        const single* data = (const single*)scalar.getDataPointer();
        return static_cast<T>(imaginary ? data[1] : data[0]);
    }
    const double* data = (const double*)scalar.getDataPointer();
    return static_cast<T>(imaginary ? data[1] : data[0]);
}
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& V, const ArrayOf& XQ)
{
    return LinearInterpolation1D(V, XQ, L"linear", L"default", nullptr);
}
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& X, const ArrayOf& V, const ArrayOf& XQ)
{
    return LinearInterpolation1D(X, V, XQ, L"linear", L"default", nullptr);
}
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& V, const ArrayOf& XQ, const std::wstring& methodName,
    const std::wstring& extrapolationMode, const ArrayOf* extrapolationValue)
{
    ArrayOf X;
    indexType len = V.isVector() ? V.getElementCount() : V.getRows();
    NelsonType destinationClass = V.isSingleClass() ? NLS_SINGLE : NLS_DOUBLE;
    if (!(V.isSingleClass() || V.isDoubleClass())) {
        Error(_W("Type not managed."));
    }
    Dimensions dims(1, len);
    if (destinationClass == NLS_SINGLE) {
        single* ptr = (single*)ArrayOf::allocateArrayOf(destinationClass, len);
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            ptr[k] = (single)(k + 1);
        }
        X = ArrayOf(destinationClass, dims, ptr);
    } else {
        double* ptr = (double*)ArrayOf::allocateArrayOf(destinationClass, len);
        OMP_PARALLEL_FOR_LOOP(len)
        for (ompIndexType k = 0; k < (ompIndexType)len; k++) {
            ptr[k] = (double)(k + 1);
        }
        X = ArrayOf(destinationClass, dims, ptr);
    }
    return LinearInterpolation1D(X, V, XQ, methodName, extrapolationMode, extrapolationValue);
}
//=============================================================================
ArrayOf
LinearInterpolation1D(const ArrayOf& X, const ArrayOf& V, const ArrayOf& XQ,
    const std::wstring& methodName, const std::wstring& extrapolationMode,
    const ArrayOf* extrapolationValue)
{
    if (!(X.isSingleClass() || X.isDoubleClass()) || !(XQ.isSingleClass() || XQ.isDoubleClass())
        || !(V.isSingleClass() || V.isDoubleClass())) {
        Error(_W("'double' or 'single' type for all input arguments expected."));
    }
    if (X.isSparse() || V.isSparse() || XQ.isSparse()) {
        Error(_W("dense type for all input arguments expected."));
    }
    if (X.isComplex() || XQ.isComplex()) {
        Error(_W("Inputs arguments must be real."));
    }
    if (!V.isVector()) {
        Error(_W("Vector V expected by native 1-D interpolation engine."));
    }
    if (X.getElementCount() != V.getElementCount()) {
        Error(_W("X and V must be of the same length."));
    }
    if (X.getElementCount() < 2) {
        Error(_W("At least two sample points expected."));
    }

    Interp1DMethod method = parseMethodName(methodName);
    ExtrapolationMode extrap = parseExtrapolationMode(extrapolationMode);
    ArrayOf x1(X);
    ArrayOf y1(V);
    ArrayOf xi(XQ);
    bool useSingle = X.isSingleClass() || V.isSingleClass() || XQ.isSingleClass();
    if (useSingle) {
        x1.promoteType(NLS_SINGLE);
        y1.promoteType(V.isComplex() ? NLS_SCOMPLEX : NLS_SINGLE);
        xi.promoteType(NLS_SINGLE);
    } else if (V.isComplex()) {
        y1.promoteType(NLS_DCOMPLEX);
    }

    ArrayOf res;
    switch (y1.getDataClass()) {
    case NLS_DOUBLE: {
        const double* x = (const double*)x1.getDataPointer();
        if (!isStrictlyIncreasing(x, x1.getElementCount())) {
            Error(_W("Sample points must be strictly increasing."));
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, xi.getElementCount());
        interpolateReal<double>(x, (const double*)y1.getDataPointer(), x1.getElementCount(),
            (const double*)xi.getDataPointer(), xi.getElementCount(), ptr, method, extrap,
            getRealScalarOrNaN<double>(extrapolationValue));
        res = ArrayOf(NLS_DOUBLE, xi.getDimensions(), ptr);
    } break;
    case NLS_SINGLE: {
        const single* x = (const single*)x1.getDataPointer();
        if (!isStrictlyIncreasing(x, x1.getElementCount())) {
            Error(_W("Sample points must be strictly increasing."));
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SINGLE, xi.getElementCount());
        interpolateReal<single>(x, (const single*)y1.getDataPointer(), x1.getElementCount(),
            (const single*)xi.getDataPointer(), xi.getElementCount(), ptr, method, extrap,
            getRealScalarOrNaN<single>(extrapolationValue));
        res = ArrayOf(NLS_SINGLE, xi.getDimensions(), ptr);
    } break;
    case NLS_DCOMPLEX: {
        const double* x = (const double*)x1.getDataPointer();
        if (!isStrictlyIncreasing(x, x1.getElementCount())) {
            Error(_W("Sample points must be strictly increasing."));
        }
        double* ptr = (double*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, xi.getElementCount());
        interpolateComplex<double>(x, (const double*)y1.getDataPointer(), x1.getElementCount(),
            (const double*)xi.getDataPointer(), xi.getElementCount(), ptr, method, extrap,
            getComplexScalarPartOrNaN<double>(extrapolationValue, false),
            getComplexScalarPartOrNaN<double>(extrapolationValue, true));
        res = ArrayOf(NLS_DCOMPLEX, xi.getDimensions(), ptr);
    } break;
    case NLS_SCOMPLEX: {
        const single* x = (const single*)x1.getDataPointer();
        if (!isStrictlyIncreasing(x, x1.getElementCount())) {
            Error(_W("Sample points must be strictly increasing."));
        }
        single* ptr = (single*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, xi.getElementCount());
        interpolateComplex<single>(x, (const single*)y1.getDataPointer(), x1.getElementCount(),
            (const single*)xi.getDataPointer(), xi.getElementCount(), ptr, method, extrap,
            getComplexScalarPartOrNaN<single>(extrapolationValue, false),
            getComplexScalarPartOrNaN<single>(extrapolationValue, true));
        res = ArrayOf(NLS_SCOMPLEX, xi.getDimensions(), ptr);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
