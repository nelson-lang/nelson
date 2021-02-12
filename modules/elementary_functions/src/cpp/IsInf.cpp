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
#include "IsInf.hpp"
#include "ClassName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
boolean_isinf(indexType N, logical* C, const T* A)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isinf(A[i]);
    }
}
//=============================================================================
template <class T>
void
boolean_isinf_cplx(indexType N, logical* C, const T* A)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = std::isinf(A[i].real()) || std::isinf(A[i].imag());
    }
}
//=============================================================================
ArrayOf
IsInf(ArrayOf A)
{
    ArrayOf C;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp
            = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        boolean_isinf<double>(A.getElementCount(), (logical*)Cp, (const double*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SINGLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount());
        boolean_isinf<single>(A.getElementCount(), (logical*)Cp, (const single*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp
            = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* pValueA = (single*)A.getDataPointer();
        auto* cplx = reinterpret_cast<singlecomplex*>(pValueA);
        boolean_isinf_cplx<singlecomplex>(A.getElementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_DCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp
            = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* pValueA = (double*)A.getDataPointer();
        auto* cplx = reinterpret_cast<doublecomplex*>(pValueA);
        boolean_isinf_cplx<doublecomplex>(A.getElementCount(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_CHAR:
    case NLS_LOGICAL:
    case NLS_INT8:
    case NLS_UINT8:
    case NLS_INT16:
    case NLS_UINT16:
    case NLS_INT32:
    case NLS_UINT32:
    case NLS_INT64:
    case NLS_UINT64: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp
            = Nelson::ArrayOf::allocateArrayOf(NLS_LOGICAL, A.getElementCount(), stringVector(), false);
        auto* CpLogical = static_cast<logical*>(Cp);
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)A.getElementCount(); i++) {
            CpLogical[i] = static_cast<logical>(0);
        }
        C.setDataPointer(Cp);
    } break;
    default: {
        Error(_("Undefined function 'isinf' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
