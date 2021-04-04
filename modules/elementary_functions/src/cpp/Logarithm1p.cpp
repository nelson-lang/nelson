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
#include "Logarithm1p.hpp"
#include "complex_abs.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
log1pComplex(Class destinationClass, T* values, bool allReal, const Dimensions& dims)
{
    ompIndexType nbElements = dims.getElementCount();
    T* ptrOut = (T*)ArrayOf::allocateArrayOf(destinationClass, nbElements);
    std::complex<T>* outZ = reinterpret_cast<std::complex<T>*>(ptrOut);
    if (allReal) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < nbElements; ++k) {
            outZ[k].real(log(complex_abs<T>(values[k] + 1, 0)));
            T imag = atan2((T)0, values[k] + 1);
            if (std::isnan(outZ[k].real()) && std::isnan(imag)) {
                outZ[k].imag(0);
            } else {
                outZ[k].imag(imag);
            }
        }
    } else {
        std::complex<T>* inZ = reinterpret_cast<std::complex<T>*>(values);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType k = 0; k < nbElements; ++k) {
            outZ[k].real(log(complex_abs<T>(inZ[k].real() + 1, inZ[k].imag())));
            outZ[k].imag(atan2(inZ[k].imag(), inZ[k].real() + 1));
        }
    }
    return ArrayOf(destinationClass, dims, ptrOut);
}
//=============================================================================
ArrayOf
Logarithm1p(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    if (A.isSparse()) {
        needToOverload = true;
        return res;
    }
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        res = log1pComplex<double>(
            NLS_DCOMPLEX, (double*)A.getDataPointer(), true, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_DCOMPLEX: {
        res = log1pComplex<double>(
            NLS_DCOMPLEX, (double*)A.getDataPointer(), false, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_DOUBLE);
        }
    } break;
    case NLS_SINGLE: {
        res = log1pComplex<single>(
            NLS_SCOMPLEX, (single*)A.getDataPointer(), true, A.getDimensions());
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        res = log1pComplex<single>(
            NLS_SCOMPLEX, (single*)A.getDataPointer(), false, A.getDimensions());
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
