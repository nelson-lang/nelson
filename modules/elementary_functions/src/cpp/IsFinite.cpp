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
#include "IsFinite.hpp"
#include "ClassName.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
void
boolean_isfinite(indexType N, logical* C, const T* A)
{
    for (indexType i = 0; i < N; i++) {
        C[i] = std::isfinite(A[i]);
    }
}
//=============================================================================
template <class T>
void
boolean_isfinite_cplx(indexType N, logical* C, const T* A)
{
    for (indexType i = 0; i < N; i++) {
        C[i] = std::isfinite(A[i].real()) && std::isfinite(A[i].imag());
    }
}
//=============================================================================
ArrayOf
IsFinite(ArrayOf A)
{
    ArrayOf C;
    switch (A.getDataClass()) {
    case NLS_DOUBLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = C.allocateArrayOf(NLS_LOGICAL, A.getLength());
        boolean_isfinite<double>(A.getLength(), (logical*)Cp, (const double*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SINGLE: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = C.allocateArrayOf(NLS_LOGICAL, A.getLength());
        boolean_isfinite<single>(A.getLength(), (logical*)Cp, (const single*)A.getDataPointer());
        C.setDataPointer(Cp);
    } break;
    case NLS_SCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = C.allocateArrayOf(NLS_LOGICAL, A.getLength());
        auto* pValueA = (single*)A.getDataPointer();
        auto* cplx = reinterpret_cast<singlecomplex*>(pValueA);
        boolean_isfinite_cplx<singlecomplex>(A.getLength(), (logical*)Cp, cplx);
        C.setDataPointer(Cp);
    } break;
    case NLS_DCOMPLEX: {
        C = ArrayOf(NLS_LOGICAL, A.getDimensions(), nullptr);
        void* Cp = C.allocateArrayOf(NLS_LOGICAL, A.getLength());
        auto* pValueA = (double*)A.getDataPointer();
        auto* cplx = reinterpret_cast<doublecomplex*>(pValueA);
        boolean_isfinite_cplx<doublecomplex>(A.getLength(), (logical*)Cp, cplx);
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
        void* Cp = C.allocateArrayOf(NLS_LOGICAL, A.getLength());
        auto* CpLogical = static_cast<logical*>(Cp);
        for (indexType i = 0; i < A.getLength(); i++) {
            CpLogical[i] = static_cast<logical>(0);
        }
        C.setDataPointer(Cp);
    } break;
    default: {
        Error(_("Undefined function 'isfinite' for input arguments of type") + " '" + ClassName(A)
            + "'.");
    } break;
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
