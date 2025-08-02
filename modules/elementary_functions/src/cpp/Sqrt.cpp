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
#include "omp_for_loop.hpp"
#include "lapack_eigen_config.hpp"
#include <limits>
#include <cmath>
#include <complex>
#include <Eigen/Dense>
#include "Sqrt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
std::complex<T>
SqrtComplexScalar(std::complex<T> z, bool& isReal)
{
    // r = sqrt((real(z) * real(z)) + (imag(z) * imag(z)))
    // phi = atan2(imag(z), real(z))
    // sqrt(r)*(cos(phi/2) + 1i*sin(phi/2))
    T realPart = 0;
    T phi = atan2(z.imag(), z.real());
    T r = sqrt((z.real() * z.real()) + (z.imag() * z.imag()));
    if (z.imag() != 0 || z.real() >= 0) {
        realPart = sqrt(r) * cos(phi / 2);
    }
    T imagPart = sqrt(r) * sin(phi / 2);
    isReal = false;
    if (fabs(imagPart) < std::numeric_limits<T>::epsilon()) {
        imagPart = (T)0.;
        isReal = true;
    }
    if (fabs(realPart) < std::numeric_limits<single>::epsilon()) {
        realPart = 0;
    }
    std::complex<T> res(realPart, imagPart);
    return res;
}
//=============================================================================
template <class T>
T
SqrtRealScalar(T value)
{
    return sqrt(value);
}
//=============================================================================
template <class T>
static bool
haveNegativeValue(T* values, indexType lengthValues)
{
    for (indexType k = 0; k < lengthValues; k++) {
        if (values[k] < 0) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
static ArrayOf
SqrtReal(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    ompIndexType elementCount = (ompIndexType)A.getElementCount();
    T* ptrIn = (T*)A.getDataPointer();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(classDestination, elementCount, stringVector(), false);
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matIn(ptrIn, elementCount);
    Eigen::Map<Eigen::Matrix<T, 1, Eigen::Dynamic>> matOut(ptrOut, elementCount);
    matOut = matIn.array().sqrt();
#else
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = SqrtRealScalar<T>(ptrIn[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SqrtComplex(NelsonType classDestination, const ArrayOf& A, bool& allReal)
{
    Dimensions dimsA = A.getDimensions();
    ompIndexType elementCount = (ompIndexType)A.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    T* ptrIn = (T*)A.getDataPointer();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    allReal = true;
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        bool isReal;
        Cz[k] = SqrtComplexScalar<T>(Az[k], isReal);
        if (!isReal) {
            allReal = false;
        }
    }
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
ArrayOf
Sqrt(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    bool asComplex = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    NelsonType classA = A.getDataClass();
    ArrayOf AA(A);

    if (classA == NLS_DOUBLE) {
        auto* ptrIn = (double*)A.getDataPointer();
        if (haveNegativeValue<double>(ptrIn, A.getElementCount())) {
            AA.promoteType(NLS_DCOMPLEX);
        }
    } else if (classA == NLS_SINGLE) {
        auto* ptrIn = (single*)A.getDataPointer();
        if (haveNegativeValue<single>(ptrIn, A.getElementCount())) {
            AA.promoteType(NLS_SCOMPLEX);
        }
    }

    NelsonType classAA = AA.getDataClass();
    switch (AA.getDataClass()) {
    case NLS_DOUBLE: {
        res = SqrtReal<double>(NLS_DOUBLE, AA);
    } break;
    case NLS_SINGLE: {
        res = SqrtReal<single>(NLS_SINGLE, AA);
    } break;
    case NLS_DCOMPLEX: {
        bool allReal = false;
        res = SqrtComplex<double>(NLS_DCOMPLEX, AA, allReal);
        if (allReal) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        bool allReal = false;
        res = SqrtComplex<single>(NLS_SCOMPLEX, AA, allReal);
        if (allReal) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    default: {
        needToOverload = true;
        return res;
    } break;
    }
    return res;
    //=============================================================================
} // namespace Nelson
//=============================================================================
} // namespace Nelson
//=============================================================================
