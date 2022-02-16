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
#include <mkl_vml.h>
#endif
#include "ComplexConjugate.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
ComplexConjugate(const ArrayOf& A)
{
    NelsonType classA = A.getDataClass();
    if (classA < NLS_LOGICAL || A.isSparse()) {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    }
    Dimensions dimsA = A.getDimensions();
    ArrayOf C;
    if (A.isEmpty()) {
        C = A;
        C.ensureSingleOwner();
        return C;
    }
    switch (classA) {
    case NLS_SCOMPLEX: {
        if (!A.isEmpty()) {
            auto* psingleA = (single*)A.getDataPointer();
            singlecomplex* ptrC
                = (singlecomplex*)ArrayOf::allocateArrayOf(NLS_SCOMPLEX, dimsA.getElementCount());
            C = ArrayOf(NLS_SCOMPLEX, dimsA, ptrC);
            ompIndexType N = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
            MKL_Complex8* ptrAz = reinterpret_cast<MKL_Complex8*>(psingleA);
            MKL_Complex8* ptrCz = reinterpret_cast<MKL_Complex8*>(ptrC);
            vcConj((MKL_INT)A.getElementCount(), ptrAz, ptrCz);
#else
            auto* Az = reinterpret_cast<singlecomplex*>(psingleA);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
            for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
                ptrC[i] = std::conj(Az[i]);
            }
#endif
        }
    } break;
    case NLS_DCOMPLEX: {
        auto* pdoubleA = (double*)A.getDataPointer();
        doublecomplex* ptrC
            = (doublecomplex*)ArrayOf::allocateArrayOf(NLS_DCOMPLEX, dimsA.getElementCount());
        C = ArrayOf(NLS_DCOMPLEX, dimsA, ptrC);
        ompIndexType N = (ompIndexType)dimsA.getElementCount();
#if defined(_NLS_WITH_VML)
        MKL_Complex16* ptrAz = reinterpret_cast<MKL_Complex16*>(pdoubleA);
        MKL_Complex16* ptrCz = reinterpret_cast<MKL_Complex16*>(ptrC);
        vzConj((MKL_INT)A.getElementCount(), ptrAz, ptrCz);
#else
#if defined(_NLS_WITH_OPENMP)
        auto* Az = reinterpret_cast<doublecomplex*>(pdoubleA);
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            ptrC[i] = std::conj(Az[i]);
        }
#endif
    } break;
    case NLS_DOUBLE:
    case NLS_SINGLE:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64: {
        // returns same value
        C = A;
        C.ensureSingleOwner();
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    default: {
        Error(_("Undefined function 'conj' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
