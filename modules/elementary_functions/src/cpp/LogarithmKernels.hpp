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
#include <complex>
#include <limits>
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "ArrayOf.hpp"
#if defined(_NLS_WITH_VML)
#include "lapack_eigen_config.hpp"
#include <mkl.h>
#include <Eigen/Dense>
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
// Shared log, log2 and log10 kernels.
enum class LogarithmBase
{
    E,
    Two,
    Ten
};
//=============================================================================
template <class T, LogarithmBase Base>
inline T
logarithmRealValue(T x)
{
    if constexpr (Base == LogarithmBase::E) {
        return std::log(x);
    } else if constexpr (Base == LogarithmBase::Two) {
        return std::log2(x);
    } else {
        return std::log10(x);
    }
}
//=============================================================================
template <class T, LogarithmBase Base>
inline T
logarithmScale()
{
    if constexpr (Base == LogarithmBase::E) {
        return (T)1;
    } else if constexpr (Base == LogarithmBase::Two) {
        return (T)1.44269504088896340735992468100189214;
    } else {
        return (T)0.434294481903251827651128918916605082;
    }
}
//=============================================================================
// Real input that has negative values: -0 stays on the real axis (log(-0) is -Inf).
template <class T, LogarithmBase Base>
inline std::complex<T>
logarithmRealAsComplex(T x)
{
    if (x < 0) {
        return std::complex<T>(logarithmRealValue<T, Base>(-x),
            (T)3.14159265358979323846264338327950288 * logarithmScale<T, Base>());
    }
    return std::complex<T>(logarithmRealValue<T, Base>(x), (T)0);
}
//=============================================================================
// Complex input: a signed zero imaginary part is read as +0, both zero parts give -Inf.
template <class T, LogarithmBase Base>
inline std::complex<T>
logarithmComplexValue(std::complex<T> z)
{
    T re = z.real();
    T im = z.imag();
    if (std::isnan(re) || std::isnan(im)) {
        T nan = std::numeric_limits<T>::quiet_NaN();
        return std::complex<T>(nan, im == 0 ? (T)0 : nan);
    }
    if (im == 0) {
        if (re < 0) {
            return logarithmRealAsComplex<T, Base>(re);
        }
        return std::complex<T>(logarithmRealValue<T, Base>(re), (T)0);
    }
    std::complex<T> w = std::log(z);
    if constexpr (Base == LogarithmBase::E) {
        return w;
    } else {
        T s = logarithmScale<T, Base>();
        return std::complex<T>(w.real() * s, w.imag() * s);
    }
}
//=============================================================================
template <class T>
inline bool
hasNegativeRealValue(const T* values, indexType count)
{
    for (indexType k = 0; k < count; k++) {
        if (values[k] < 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Dense log of a real or complex array; complex input keeps a complex result unless
// promoteAllReal is set.
template <class T, LogarithmBase Base>
ArrayOf
logarithmDense(const ArrayOf& A, NelsonType realClass, NelsonType complexClass, bool promoteAllReal)
{
    Dimensions dimsA = A.dimensions();
    ompIndexType elementCount = (ompIndexType)dimsA.elementCount();
    if (A.dataClass() == realClass) {
        const T* ptrIn = (const T*)A.dataPointer();
        if (!hasNegativeRealValue<T>(ptrIn, elementCount)) {
            T* ptrOut = (T*)ArrayOf::allocateArrayOf(realClass, elementCount, stringVector(), false);
#if defined(_NLS_WITH_VML)
            if constexpr (Base == LogarithmBase::E) {
                Eigen::Map<const Eigen::Array<T, Eigen::Dynamic, 1>> matIn(ptrIn, elementCount);
                Eigen::Map<Eigen::Array<T, Eigen::Dynamic, 1>> matOut(ptrOut, elementCount);
                matOut = matIn.log();
                return ArrayOf(realClass, dimsA, ptrOut);
            }
#endif
            OMP_PARALLEL_FOR_LOOP(elementCount)
            for (ompIndexType k = 0; k < elementCount; k++) {
                ptrOut[k] = logarithmRealValue<T, Base>(ptrIn[k]);
            }
            return ArrayOf(realClass, dimsA, ptrOut);
        }
        T* ptrOut = (T*)ArrayOf::allocateArrayOf(complexClass, elementCount, stringVector(), false);
        std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            Cz[k] = logarithmRealAsComplex<T, Base>(ptrIn[k]);
        }
        return ArrayOf(complexClass, dimsA, ptrOut);
    }
    const std::complex<T>* Az = reinterpret_cast<const std::complex<T>*>(A.dataPointer());
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(complexClass, elementCount, stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>(ptrOut);
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = logarithmComplexValue<T, Base>(Az[k]);
    }
    ArrayOf res(complexClass, dimsA, ptrOut);
    if (promoteAllReal && res.allReal()) {
        res.promoteType(realClass);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
