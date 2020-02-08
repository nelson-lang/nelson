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
#include <Eigen/Dense>
#include <complex>
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
SqrtReal(Class classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    T* ptrIn = (T*)A.getDataPointer();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    ompIndexType elementCount = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < elementCount; k++) {
        ptrOut[k] = SqrtRealScalar<T>(ptrIn[k]);
    }
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
template <class T>
static ArrayOf
SqrtComplex(Class classDestination, const ArrayOf& A)
{
    Dimensions dimsA = A.getDimensions();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(
        classDestination, dimsA.getElementCount(), stringVector(), false);
    std::complex<T>* Cz = reinterpret_cast<std::complex<T>*>((T*)ptrOut);
    T* ptrIn = (T*)A.getDataPointer();
    std::complex<T>* Az = reinterpret_cast<std::complex<T>*>((T*)A.getDataPointer());
    ompIndexType elementCount = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType k = 0; k < elementCount; k++) {
        Cz[k] = SqrtComplexScalar<T>(Az[k]);
    }
    return ArrayOf(classDestination, dimsA, ptrOut);
}
//=============================================================================
ArrayOf
Sqrt(ArrayOf A, bool& needToOverload)
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
                    res = SqrtComplex<double>(NLS_DCOMPLEX, A);
                } else {
                    res = SqrtReal<double>(NLS_DOUBLE, A);
                }
            } else {
                res = SqrtComplex<double>(NLS_DCOMPLEX, A);
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
                    res = SqrtComplex<single>(NLS_SCOMPLEX, A);
                } else {
                    res = SqrtReal<single>(NLS_SINGLE, A);
                }
            } else {
                res = SqrtComplex<single>(NLS_SCOMPLEX, A);
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
