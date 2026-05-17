//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#if !WITH_OPENMP
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#endif
#include <cmath>
#include <limits>
#include "ErrorFunctions.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
static T
normalInverse(T p)
{
    const T a[] = { (T)-3.969683028665376e+01, (T)2.209460984245205e+02, (T)-2.759285104469687e+02,
        (T)1.383577518672690e+02, (T)-3.066479806614716e+01, (T)2.506628277459239e+00 };
    const T b[] = { (T)-5.447609879822406e+01, (T)1.615858368580409e+02, (T)-1.556989798598866e+02,
        (T)6.680131188771972e+01, (T)-1.328068155288572e+01 };
    const T c[] = { (T)-7.784894002430293e-03, (T)-3.223964580411365e-01, (T)-2.400758277161838e+00,
        (T)-2.549732539343734e+00, (T)4.374664141464968e+00, (T)2.938163982698783e+00 };
    const T d[] = { (T)7.784695709041462e-03, (T)3.224671290700398e-01, (T)2.445134137142996e+00,
        (T)3.754408661907416e+00 };

    const T plow = (T)0.02425;
    const T phigh = (T)1 - plow;
    T q;

    if (p < plow) {
        q = std::sqrt((T)-2 * std::log(p));
        return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
            / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + (T)1);
    }
    if (p <= phigh) {
        q = p - (T)0.5;
        T r = q * q;
        return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q
            / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + (T)1);
    }

    q = std::sqrt((T)-2 * std::log((T)1 - p));
    return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5])
        / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + (T)1);
}
//=============================================================================
template <typename T>
static T
refineErfInv(T y, T x)
{
    const T twoOverSqrtPi = (T)1.12837916709551257389615890312154517169;
    for (int k = 0; k < 2; ++k) {
        T derivative = twoOverSqrtPi * std::exp(-y * y);
        if (derivative == (T)0) {
            break;
        }
        y -= (std::erf(y) - x) / derivative;
    }
    return y;
}
//=============================================================================
template <typename T>
static T
refineErfcInv(T y, T x)
{
    const T twoOverSqrtPi = (T)1.12837916709551257389615890312154517169;
    for (int k = 0; k < 2; ++k) {
        T derivative = -twoOverSqrtPi * std::exp(-y * y);
        if (derivative == (T)0) {
            break;
        }
        y -= (std::erfc(y) - x) / derivative;
    }
    return y;
}
//=============================================================================
template <typename T>
static T
erfinvValue(T v)
{
    if (std::isnan(v)) {
        return std::numeric_limits<T>::quiet_NaN();
    }
    if (v < (T)-1 || v > (T)1) {
        return std::numeric_limits<T>::quiet_NaN();
    }
    if (v == (T)-1) {
        return -std::numeric_limits<T>::infinity();
    }
    if (v == (T)1) {
        return std::numeric_limits<T>::infinity();
    }
    T y = normalInverse(((T)1 + v) / (T)2) / std::sqrt((T)2);
    return refineErfInv(y, v);
}
//=============================================================================
template <typename T>
static T
erfcinvValue(T v)
{
    if (std::isnan(v)) {
        return std::numeric_limits<T>::quiet_NaN();
    }
    if (v < (T)0 || v > (T)2) {
        return std::numeric_limits<T>::quiet_NaN();
    }
    if (v == (T)0) {
        return std::numeric_limits<T>::infinity();
    }
    if (v == (T)2) {
        return -std::numeric_limits<T>::infinity();
    }
    T y = -normalInverse(v / (T)2) / std::sqrt((T)2);
    return refineErfcInv(y, v);
}
//=============================================================================
template <typename T>
static T
erfcxPositive(T v)
{
    if (v > (T)26) {
        const T inv = (T)1 / v;
        const T inv2 = inv * inv;
        const T invSqrtPi = (T)0.56418958354775628694807945156077258584;
        return invSqrtPi * inv * ((T)1 + inv2 * ((T)-0.5 + inv2 * ((T)0.75 + inv2 * (T)-1.875)));
    }
    return std::exp(v * v) * std::erfc(v);
}
//=============================================================================
template <typename T>
static T
erfcxValue(T v)
{
    if (std::isnan(v)) {
        return std::numeric_limits<T>::quiet_NaN();
    }
    if (std::isinf(v)) {
        return (v > (T)0) ? (T)0 : std::numeric_limits<T>::infinity();
    }
    if (v < (T)0) {
        const T av = -v;
        if (av > std::sqrt(std::log(std::numeric_limits<T>::max() / (T)2))) {
            return std::numeric_limits<T>::infinity();
        }
        return (T)2 * std::exp(av * av) - erfcxPositive(av);
    }
    return erfcxPositive(v);
}
//=============================================================================
template <typename T>
static T
errorFunctionValue(T v, ErrorFunctionType functionType)
{
    switch (functionType) {
    case ErrorFunctionType::Erf:
        return std::erf(v);
    case ErrorFunctionType::Erfc:
        return std::erfc(v);
    case ErrorFunctionType::ErfInv:
        return erfinvValue(v);
    case ErrorFunctionType::ErfcInv:
        return erfcinvValue(v);
    case ErrorFunctionType::Erfcx:
        return erfcxValue(v);
    }
    return std::numeric_limits<T>::quiet_NaN();
}
//=============================================================================
static double
errorFunctionDouble(double v, ErrorFunctionType functionType)
{
    return errorFunctionValue(v, functionType);
}
//=============================================================================
static single
errorFunctionSingle(single v, ErrorFunctionType functionType)
{
    return errorFunctionValue(v, functionType);
}
//=============================================================================
ArrayOf
ErrorFunction(const ArrayOf& arrayIn, ErrorFunctionType functionType)
{
    ArrayOf res;
    if (!arrayIn.isEmpty()) {
        Dimensions dimsIn = arrayIn.getDimensions();
        if (arrayIn.getDataClass() == NLS_DOUBLE) {
            double* ptrOut = (double*)ArrayOf::allocateArrayOf(
                NLS_DOUBLE, dimsIn.getElementCount(), stringVector(), false);
            auto* ptrIn = (double*)arrayIn.getDataPointer();
            indexType elementCount = dimsIn.getElementCount();
#if WITH_OPENMP
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
                ptrOut[k] = errorFunctionDouble(ptrIn[k], functionType);
            }
#else
            Eigen::Map<Eigen::ArrayXd> matOut(ptrOut, elementCount);
            Eigen::Map<Eigen::ArrayXd> matIn(ptrIn, elementCount);
            matOut = matIn.unaryExpr(
                [functionType](double value) { return errorFunctionDouble(value, functionType); });
#endif
            res = ArrayOf(NLS_DOUBLE, dimsIn, ptrOut);
        } else {
            single* ptrOut = (single*)ArrayOf::allocateArrayOf(
                NLS_SINGLE, dimsIn.getElementCount(), stringVector(), false);
            auto* ptrIn = (single*)arrayIn.getDataPointer();
            indexType elementCount = dimsIn.getElementCount();
#if WITH_OPENMP
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < (ompIndexType)elementCount; ++k) {
                ptrOut[k] = errorFunctionSingle(ptrIn[k], functionType);
            }
#else
            Eigen::Map<Eigen::ArrayXf> matOut(ptrOut, elementCount);
            Eigen::Map<Eigen::ArrayXf> matIn(ptrIn, elementCount);
            matOut = matIn.unaryExpr(
                [functionType](single value) { return errorFunctionSingle(value, functionType); });
#endif
            res = ArrayOf(NLS_SINGLE, dimsIn, ptrOut);
        }
    } else {
        res = arrayIn;
        res.ensureSingleOwner();
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
