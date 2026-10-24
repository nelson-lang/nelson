//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _SCL_SECURE_NO_WARNINGS
#endif
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#if WITH_OPENMP
#include <omp.h>
#endif
#include <algorithm>
#include <complex>
#include <cstring>
#include "LapackPrototypes.hpp"
#include "CholeskyFactorization.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#if WITH_OPENMP
class ScopedOpenMPThreads
{
    int previousThreadCount = 1;
    bool changed = false;

public:
    explicit ScopedOpenMPThreads(int requestedThreadCount)
    {
        previousThreadCount = omp_get_max_threads();
        if (requestedThreadCount > 0 && requestedThreadCount < previousThreadCount) {
            omp_set_num_threads(requestedThreadCount);
            changed = true;
        }
    }

    ~ScopedOpenMPThreads()
    {
        if (changed) {
            omp_set_num_threads(previousThreadCount);
        }
    }
};
//=============================================================================
static int
choleskyThreadCount(int leadDim)
{
    int currentThreadCount = omp_get_max_threads();
    if (leadDim <= 1024 && currentThreadCount > 1) {
        return 1;
    }
    return currentThreadCount;
}
#else
class ScopedOpenMPThreads
{
public:
    explicit ScopedOpenMPThreads(int) { }
};
//=============================================================================
static int
choleskyThreadCount(int)
{
    return 1;
}
#endif
//=============================================================================
template <class T>
static bool
isFiniteValue(T value)
{
    return std::isfinite(value);
}
//=============================================================================
template <class T>
static bool
isFiniteValue(const std::complex<T>& value)
{
    return std::isfinite(value.real()) && std::isfinite(value.imag());
}
//=============================================================================
template <class T>
static int
firstNonFiniteMinor(const T* values, indexType order, bool lowerTriangle)
{
    int first = 0;
    for (indexType col = 0; col < order; ++col) {
        indexType firstRow = lowerTriangle ? col : 0;
        indexType lastRow = lowerTriangle ? order : col + 1;
        for (indexType row = firstRow; row < lastRow; ++row) {
            if (!isFiniteValue(values[row + col * order])) {
                int minor = static_cast<int>(std::max(row, col) + 1);
                first = first == 0 ? minor : std::min(first, minor);
            }
        }
    }
    return first;
}
//=============================================================================
static int
firstNonFiniteMinor(const ArrayOf& A, bool lowerTriangle)
{
    indexType order = A.columns();
    if (A.isComplex()) {
        if (A.isDoubleClass()) {
            auto* values = reinterpret_cast<const std::complex<double>*>(
                static_cast<const double*>(A.dataPointer()));
            return firstNonFiniteMinor(values, order, lowerTriangle);
        }
        auto* values = reinterpret_cast<const std::complex<single>*>(
            static_cast<const single*>(A.dataPointer()));
        return firstNonFiniteMinor(values, order, lowerTriangle);
    }
    if (A.isDoubleClass()) {
        return firstNonFiniteMinor(
            static_cast<const double*>(A.dataPointer()), order, lowerTriangle);
    }
    return firstNonFiniteMinor(static_cast<const single*>(A.dataPointer()), order, lowerTriangle);
}
//=============================================================================
template <class T>
static ArrayOf
leadingSubmatrix(const ArrayOf& A, indexType order, NelsonType dataClass)
{
    if (order == 0) {
        return ArrayOf::empty();
    }
    T* dst = reinterpret_cast<T*>(
        ArrayOf::allocateArrayOf(dataClass, order * order, stringVector(), false));
    const T* src = reinterpret_cast<const T*>(A.dataPointer());
    indexType sourceOrder = A.columns();
    for (indexType col = 0; col < order; ++col) {
        std::copy(src + col * sourceOrder, src + col * sourceOrder + order, dst + col * order);
    }
    return ArrayOf(dataClass, Dimensions(order, order), dst);
}
//=============================================================================
static ArrayOf
leadingSubmatrix(const ArrayOf& A, indexType order)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return leadingSubmatrix<double>(A, order, NLS_DOUBLE);
    case NLS_SINGLE:
        return leadingSubmatrix<single>(A, order, NLS_SINGLE);
    case NLS_DCOMPLEX:
        return leadingSubmatrix<std::complex<double>>(A, order, NLS_DCOMPLEX);
    case NLS_SCOMPLEX:
        return leadingSubmatrix<std::complex<single>>(A, order, NLS_SCOMPLEX);
    default:
        return {};
    }
}
//=============================================================================
template <class T>
static ArrayOf
leadingFactor(const ArrayOf& A, indexType order, bool lowerTriangle, NelsonType dataClass)
{
    if (order == 0) {
        return ArrayOf::empty();
    }
    T* dst = reinterpret_cast<T*>(
        ArrayOf::allocateArrayOf(dataClass, order * order, stringVector(), true));
    const T* src = reinterpret_cast<const T*>(A.dataPointer());
    indexType sourceOrder = A.columns();
    for (indexType col = 0; col < order; ++col) {
        indexType firstRow = lowerTriangle ? col : 0;
        indexType lastRow = lowerTriangle ? order : col + 1;
        for (indexType row = firstRow; row < lastRow; ++row) {
            dst[row + col * order] = src[row + col * sourceOrder];
        }
    }
    return ArrayOf(dataClass, Dimensions(order, order), dst);
}
//=============================================================================
static ArrayOf
leadingFactor(const ArrayOf& A, indexType order, bool lowerTriangle)
{
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        return leadingFactor<double>(A, order, lowerTriangle, NLS_DOUBLE);
    case NLS_SINGLE:
        return leadingFactor<single>(A, order, lowerTriangle, NLS_SINGLE);
    case NLS_DCOMPLEX:
        return leadingFactor<std::complex<double>>(A, order, lowerTriangle, NLS_DCOMPLEX);
    case NLS_SCOMPLEX:
        return leadingFactor<std::complex<single>>(A, order, lowerTriangle, NLS_SCOMPLEX);
    default:
        return {};
    }
}
//=============================================================================
int
singleRealCholeskyFactorization(bool lowerTriangle, single* ptrS, int leadDim)
{
    int info;
    char orient = lowerTriangle ? 'L' : 'U';
    ScopedOpenMPThreads scopedThreads(choleskyThreadCount(leadDim));
    LAPACK_spotrf(&orient, &leadDim, ptrS, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
#if WITH_OPENMP
#pragma omp parallel for if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (int col = 0; col < leadDim; col++) {
            int firstRow = lowerTriangle ? 0 : col + 1;
            int lastRow = lowerTriangle ? col : leadDim;
            int count = lastRow - firstRow;
            if (count > 0) {
                memset(ptrS + firstRow + col * leadDim, 0, (size_t)count * sizeof(single));
            }
        }
    }
    return 0;
}
//=============================================================================
int
singleComplexCholeskyFactorization(bool lowerTriangle, std::complex<single>* ptrZ, int leadDim)
{
    int info = 0;
    char orient = lowerTriangle ? 'L' : 'U';
    ScopedOpenMPThreads scopedThreads(choleskyThreadCount(leadDim));
    LAPACK_cpotrf(&orient, &leadDim, ptrZ, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
#if WITH_OPENMP
#pragma omp parallel for if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (int col = 0; col < leadDim; col++) {
            int firstRow = lowerTriangle ? 0 : col + 1;
            int lastRow = lowerTriangle ? col : leadDim;
            for (int row = firstRow; row < lastRow; row++) {
                ptrZ[row + col * leadDim].real(0);
                ptrZ[row + col * leadDim].imag(0);
            }
        }
    }
    return 0;
}
//=============================================================================
static int
doubleRealCholeskyFactorization(bool lowerTriangle, double* ptrD, int leadDim)
{
    int info;
    char orient = lowerTriangle ? 'L' : 'U';
    ScopedOpenMPThreads scopedThreads(choleskyThreadCount(leadDim));
    LAPACK_dpotrf(&orient, &leadDim, ptrD, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
#if WITH_OPENMP
#pragma omp parallel for if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (int col = 0; col < leadDim; col++) {
            int firstRow = lowerTriangle ? 0 : col + 1;
            int lastRow = lowerTriangle ? col : leadDim;
            int count = lastRow - firstRow;
            if (count > 0) {
                memset(ptrD + firstRow + col * leadDim, 0, (size_t)count * sizeof(double));
            }
        }
    }
    return 0;
}
//=============================================================================
static int
doubleComplexCholeskyFactorization(bool lowerTriangle, std::complex<double>* ptrZ, int leadDim)
{
    int info = 0;
    char orient = lowerTriangle ? 'L' : 'U';
    ScopedOpenMPThreads scopedThreads(choleskyThreadCount(leadDim));
    LAPACK_zpotrf(&orient, &leadDim, ptrZ, &leadDim, &info);
    if (info > 0 || info < 0) {
        return info;
    }
    if (leadDim > 1) {
        const std::complex<double> zero(0.0, 0.0);
#if WITH_OPENMP
#pragma omp parallel for if (leadDim > OMP_DEFAULT_THRESHOLD)
#endif
        for (int col = 0; col < leadDim; col++) {
            int firstRow = lowerTriangle ? 0 : col + 1;
            int lastRow = lowerTriangle ? col : leadDim;
            for (int row = firstRow; row < lastRow; row++) {
                ptrZ[row + col * leadDim] = zero;
            }
        }
    }
    return 0;
}
//=============================================================================
static int
factorizeInPlace(ArrayOf& R, bool lowerTriangle)
{
    int info = 0;
    int order = static_cast<int>(R.columns());
    if (R.isComplex()) {
        if (R.isDoubleClass()) {
            double* ptr = (double*)R.readWriteDataPointer();
            auto* ptrZ = reinterpret_cast<std::complex<double>*>(ptr);
            info = doubleComplexCholeskyFactorization(lowerTriangle, ptrZ, order);
        } else {
            single* ptr = (single*)R.readWriteDataPointer();
            auto* ptrZ = reinterpret_cast<std::complex<single>*>(ptr);
            info = singleComplexCholeskyFactorization(lowerTriangle, ptrZ, order);
        }
        if (R.allReal()) {
            R.promoteType(R.isDoubleClass() ? NLS_DOUBLE : NLS_SINGLE);
        }
    } else if (R.isDoubleClass()) {
        double* ptr = (double*)R.readWriteDataPointer();
        info = doubleRealCholeskyFactorization(lowerTriangle, ptr, order);
    } else {
        single* ptr = (single*)R.readWriteDataPointer();
        info = singleRealCholeskyFactorization(lowerTriangle, ptr, order);
    }
    return info;
}
//=============================================================================
static ArrayOf
factorizedLeadingSubmatrix(const ArrayOf& A, indexType order, bool lowerTriangle)
{
    ArrayOf R = leadingSubmatrix(A, order);
    if (R.isEmpty()) {
        return R;
    }
    int info = factorizeInPlace(R, lowerTriangle);
    if (info < 0) {
        Error(L"Nelson:chol:lapackIllegalValue", L"One argument had an illegal value.");
    }
    if (info > 0) {
        return leadingFactor(R, info - 1, lowerTriangle);
    }
    return R;
}
//=============================================================================
ArrayOf
CholeskyFactorization(const ArrayOf& A, bool lowerTriangle, bool& needToOverload, int& info)
{
    needToOverload = false;
    info = 0;
    bool isSupportedTypes = (A.isDoubleClass() || A.isSingleClass()) && !A.isSparse();
    if (!isSupportedTypes) {
        needToOverload = true;
        return {};
    }
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    if (A.isEmpty()) {
        ArrayOf res = ArrayOf::empty();
        if (A.isSingleType()) {
            res.promoteType(NLS_SINGLE);
        }
        return res;
    }
    info = firstNonFiniteMinor(A, lowerTriangle);
    ArrayOf R = info == 0 ? A : leadingSubmatrix(A, info - 1);
    if (R.isEmpty()) {
        return R;
    }
    int factorizationInfo = factorizeInPlace(R, lowerTriangle);
    if (factorizationInfo < 0) {
        Error(L"Nelson:chol:lapackIllegalValue", L"One argument had an illegal value.");
    }
    if (factorizationInfo > 0) {
        info = factorizationInfo;
        return factorizedLeadingSubmatrix(A, info - 1, lowerTriangle);
    }
    if (info != 0) {
        return R;
    }
    return R;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
