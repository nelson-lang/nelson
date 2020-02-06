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
#include <Eigen/Dense>
#include <complex>
#include "Logarithm1p.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
log1pComplex(Class destinationClass, T* values, Dimensions& dims)
{
    std::complex<T>* cz = reinterpret_cast<std::complex<T>*>(values);
    for (indexType k = 0; k < dims.getElementCount(); ++k) {
        std::complex<T> current = cz[k];
        if (current.imag() == 0.) {
            std::complex<T> v(std::log1p(cz[k].real()), 0);
            cz[k] = v;
        } else {
            Eigen::Map<Eigen::Matrix<std::complex<T>, Eigen::Dynamic, Eigen::Dynamic>> mat(
                &cz[k], 1, 1);
            mat = mat.array().log1p();
        }
    }
}
//=============================================================================
ArrayOf
Logarithm1p(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf res;
    needToOverload = false;
    Class classA = A.getDataClass();
    switch (classA) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.getDimensions();
            res = ArrayOf(A);
            res.promoteType(NLS_DCOMPLEX);
            log1pComplex<double>(NLS_DCOMPLEX, (double*)res.getDataPointer(), dimsA);
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            needToOverload = true;
        } else {
            Dimensions dimsA = A.getDimensions();
            res = ArrayOf(A);
            res.ensureSingleOwner();
            log1pComplex<double>(NLS_DCOMPLEX, (double*)res.getDataPointer(), dimsA);
            if (res.allReal()) {
                res.promoteType(NLS_DOUBLE);
            }
        }
    } break;
    case NLS_SINGLE: {
        Dimensions dimsA = A.getDimensions();
        res = ArrayOf(A);
        res.promoteType(NLS_SCOMPLEX);
        log1pComplex<single>(NLS_SCOMPLEX, (single*)res.getDataPointer(), dimsA);
        if (res.allReal()) {
            res.promoteType(NLS_SINGLE);
        }
    } break;
    case NLS_SCOMPLEX: {
        Dimensions dimsA = A.getDimensions();
        res = ArrayOf(A);
        res.ensureSingleOwner();
        log1pComplex<single>(NLS_SCOMPLEX, (single*)res.getDataPointer(), dimsA);
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
