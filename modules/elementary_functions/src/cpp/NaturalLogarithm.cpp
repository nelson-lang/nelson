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
#if defined(_NLS_WITH_VML)
#include <mkl.h>
#endif
#include <Eigen/Dense>
#include <complex>
#include "NaturalLogarithm.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
std::complex<T>
NaturalLogarithmComplexScalar(std::complex<T> z)
{
    T sqrtRes = sqrt((z.real() * z.real()) + (z.imag() * z.imag()));
    T angleRes = atan2(z.imag(), z.real());
    std::complex<T> res(log(sqrtRes), angleRes);
    return res;
}
//=============================================================================
template <class T>
T
NaturalLogarithmRealScalar(T value)
{
    return log(value);
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
NaturalLogarithmReal(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    T* ptrIn = (T*)A.dataPointer();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.elementCount(), stringVector(), false);
    ompIndexType elementCount = (ompIndexType)dimsA.elementCount();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matEigen(ptrIn, elementCount);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(ptrOut, elementCount);
    matRes = matEigen.array().log();
#else
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = NaturalLogarithmRealScalar<T>(ptrIn[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
NaturalLogarithmComplex(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.dimensions();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.elementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.dataPointer());
    ompIndexType elementCount = (ompIndexType)dimsA.elementCount();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matEigen(Az, elementCount);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matRes(Cz, elementCount);
    matRes = matEigen.array().log();
#else
    OMP_PARALLEL_FOR_LOOP(elementCount)
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = NaturalLogarithmComplexScalar<T>(Az[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
ArrayOf
NaturalLogarithm(ArrayOf A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    NelsonType classA = A.dataClass();
    if (classA == NLS_DOUBLE || classA == NLS_DCOMPLEX) {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.dimensions();
            if (classA == NLS_DOUBLE) {
                auto* ptrIn = (double*)A.dataPointer();
                if (haveNegativeValue<double>(ptrIn, dimsA.elementCount())) {
                    A.promoteType(NLS_DCOMPLEX);
                    res = NaturalLogarithmComplex<double>(NLS_DCOMPLEX, A);
                } else {
                    res = NaturalLogarithmReal<double>(NLS_DOUBLE, A);
                }
            } else {
                res = NaturalLogarithmComplex<double>(NLS_DCOMPLEX, A);
            }
        }
    } else if (classA == NLS_SINGLE || classA == NLS_SCOMPLEX) {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.dimensions();
            if (classA == NLS_SINGLE) {
                auto* ptrIn = (single*)A.dataPointer();
                if (haveNegativeValue<single>(ptrIn, dimsA.elementCount())) {
                    A.promoteType(NLS_SCOMPLEX);
                    res = NaturalLogarithmComplex<single>(NLS_SCOMPLEX, A);
                } else {
                    res = NaturalLogarithmReal<single>(NLS_SINGLE, A);
                }
            } else {
                res = NaturalLogarithmComplex<single>(NLS_SCOMPLEX, A);
            }
        }
    } else {
        needToOverload = true;
    }
    return res;
} // namespace Nelson
//=============================================================================
} // namespace Nelson
//=============================================================================
