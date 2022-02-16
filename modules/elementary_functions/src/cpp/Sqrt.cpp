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
#include "Sqrt.hpp"
#include <complex>
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
std::complex<T>
SqrtComplexScalar(std::complex<T> z)
{
    // r = sqrt((real(z) * real(z)) + (imag(z) * imag(z)))
    // phi = atan2(imag(z), real(z))
    // sqrt(r)*(cos(phi/2) + 1i*sin(phi/2))
    if (z.imag() == 0 && z.real() < 0) {
        T r = sqrt((z.real() * z.real()) + (z.imag() * z.imag()));
        T phi = atan2(z.imag(), z.real());
        std::complex<T> res(0, sqrt(r) * sin(phi / 2));
        return res;
    }
    T r = sqrt((z.real() * z.real()) + (z.imag() * z.imag()));
    T phi = atan2(z.imag(), z.real());
    std::complex<T> res(sqrt(r) * cos(phi / 2), sqrt(r) * sin(phi / 2));
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = SqrtRealScalar<T>(ptrIn[k]);
    }
#endif
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SqrtComplex(NelsonType classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    ompIndexType elementCount = (ompIndexType)A.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    T* ptrIn = (T*)A.getDataPointer();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
#if defined(_NLS_WITH_VML)
    Eigen::Map<Eigen::Matrix<std::complex<T>, 1, Eigen::Dynamic>> matIn(Az, elementCount);
    Eigen::Map<Eigen::Matrix<std::complex<T>, 1, Eigen::Dynamic>> matOut(Cz, elementCount);
    matOut = matIn.array().sqrt();
#else
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = SqrtComplexScalar<T>(Az[k]);
    }
#endif
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
        res = SqrtComplex<double>(NLS_DCOMPLEX, AA);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = SqrtComplex<single>(NLS_SCOMPLEX, AA);
        if (res.allReal()) {
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
