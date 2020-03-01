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
#include "Mean.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
T
TMeanAllReal(const T* spx, indexType elementCount)
{
    T sum = 0.;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sum)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount; ++i) {
        sum += (T)spx[i];
    }
    return sum / (T)elementCount;
}
//=============================================================================
template <class T>
std::complex<T>
TMeanAllComplex(const T* spx, indexType elementCount)
{
    std::complex<T> sum;
    T sumImag = 0.;
    T sumReal = 0.;

#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for reduction(+ : sumImag, sumReal)
#endif
    for (ompIndexType i = 0; i < (ompIndexType)elementCount * 2; i = i + 2) {
        sumReal += (T)spx[i];
        sumImag += (T)spx[i + 1];
    }
    std::complex<T> m(sumReal / (T)elementCount, sumImag / (T)elementCount);
    return m;
}
//=============================================================================
ArrayOf
Mean(const ArrayOf& A, bool& needToOverload)
{
    ArrayOfVector retval;
    ArrayOf res;
    needToOverload = false;
    if (A.isEmpty()) {
        Dimensions dims(1, 0);
        res = ArrayOf::emptyConstructor(dims, A.isSparse());
        return res;
    }
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    if (A.isScalar()) {
        res = A;
        res.promoteType(NLS_DOUBLE);
        return res;
    }
    Dimensions dimsOut(1, 1);
    switch (A.getDataClass()) {
    case NLS_HANDLE:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_STRING_ARRAY:
    default: {
        needToOverload = true;
    } break;
    case NLS_LOGICAL: {

    } break;
    case NLS_UINT8: {
    } break;
    case NLS_INT8: {
    } break;
    case NLS_UINT16: {
    } break;
    case NLS_INT16: {
    } break;
    case NLS_UINT32: {
    } break;
    case NLS_INT32: {
    } break;
    case NLS_UINT64: {
    } break;
    case NLS_INT64: {
    } break;
    case NLS_SINGLE: {
        indexType elementCount = A.getDimensions().getElementCount();
        single m = TMeanAllReal<single>((single*)A.getDataPointer(), elementCount);
        res = ArrayOf::singleConstructor(m);
    } break;
    case NLS_DOUBLE: {
        indexType elementCount = A.getDimensions().getElementCount();
        double m = TMeanAllReal<double>((double*)A.getDataPointer(), elementCount);
        res = ArrayOf::doubleConstructor(m);
    } break;
    case NLS_SCOMPLEX: {
        indexType elementCount = A.getDimensions().getElementCount();
        std::complex<single> m = TMeanAllComplex<single>((single*)A.getDataPointer(), elementCount);
        res = ArrayOf::complexConstructor(m.real(), m.imag());
    } break;
    case NLS_DCOMPLEX: {
        indexType elementCount = A.getDimensions().getElementCount();
        std::complex<double> m = TMeanAllComplex<double>((double*)A.getDataPointer(), elementCount);
        res = ArrayOf::dcomplexConstructor(m.real(), m.imag());
    } break;
    case NLS_CHAR: {
    } break;
    }
    return res;
}
//=============================================================================
}
//=============================================================================
