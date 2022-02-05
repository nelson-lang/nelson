//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsConfig.h"
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
    // log(sqrt((real(z) * real(z)) + (imag(z) * imag(z)) + (i * atan2(imag(z),
    // real(z)))
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
NaturalLogarithmReal(Class classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    T* ptrIn = (T*)A.getDataPointer();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    ompIndexType elementCount = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matEigen(ptrIn, elementCount);
    Eigen::Map<Eigen::Matrix<T, Eigen::Dynamic, 1>> matRes(ptrOut, elementCount);
    matRes = matEigen.array().log();
#else
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = NaturalLogarithmRealScalar<T>(ptrIn[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
NaturalLogarithmComplex(Class classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    ompIndexType elementCount = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matEigen(Az, elementCount);
    Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, 1>> matRes(Cz, elementCount);
    matRes = matEigen.array().log();
#else
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
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
    Class classA = A.getDataClass();
    if (classA == NLS_DOUBLE || classA == NLS_DCOMPLEX) {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.getDimensions();
            if (classA == NLS_DOUBLE) {
                auto* ptrIn = (double*)A.getDataPointer();
                if (haveNegativeValue<double>(ptrIn, dimsA.getElementCount())) {
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
            Dimensions dimsA = A.getDimensions();
            if (classA == NLS_SINGLE) {
                auto* ptrIn = (single*)A.getDataPointer();
                if (haveNegativeValue<single>(ptrIn, dimsA.getElementCount())) {
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
