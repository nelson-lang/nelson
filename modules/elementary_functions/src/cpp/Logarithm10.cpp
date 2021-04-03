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
#include <complex>
#include "Logarithm10.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
log10Complex(Class destinationClass, T* values, bool allReal, Dimensions& dims)
{
    ompIndexType elementCount = dims.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(destinationClass, elementCount);
    std::complex<T>* outZ = reinterpret_cast<std::complex<T>*>(ptrOut);
    if (allReal) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            std::complex<T> current(values[k], (T)0);
            outZ[k] = std::log10(current);
            if (std::isnan(outZ[k].imag())) {
                outZ[k].imag((T)0);
            }
        }
    } else {
        std::complex<T>* Az = reinterpret_cast<std::complex<T>*>(values);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < elementCount; ++k) {
            outZ[k] = std::log10(Az[k]);
        }
    }
    return ArrayOf(destinationClass, dims, ptrOut);
}
//=============================================================================
ArrayOf
Logarithm10(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    needToOverload = false;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<double>(NLS_DCOMPLEX, (double*)A.getDataPointer(), true, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<double>(NLS_DCOMPLEX, (double*)A.getDataPointer(), false, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<single>(NLS_SCOMPLEX, (single*)A.getDataPointer(), true, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        res = log10Complex<single>(NLS_SCOMPLEX, (single*)res.getDataPointer(), false, dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    default: {
        needToOverload = true;
    } break;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
