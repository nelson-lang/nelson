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
#include "Negate.hpp"
//=============================================================================
namespace Nelson {
template <class T>
void
negate(indexType N, T* C, const T* A)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = -A[i];
    }
}
//=============================================================================
ArrayOf
Negate(ArrayOf A)
{
    ArrayOf C;
    Class Aclass;
    if (A.isReferenceType()) {
        Error(L"Cannot negate non-numeric types.");
    }
    Aclass = A.getDataClass();
    if (A.isRowVectorCharacterArray()) {
        Aclass = NLS_DOUBLE;
    } else if (Aclass < NLS_INT32) {
        Aclass = NLS_INT32;
    }
    A.promoteType(Aclass);
    C = ArrayOf(Aclass, A.getDimensions(), nullptr);
    void* Cp = Nelson::ArrayOf::allocateArrayOf(Aclass, A.getElementCount(), stringVector(), false);
    switch (Aclass) {
    case NLS_INT32:
        negate<int32>(A.getElementCount(), static_cast<int32*>(Cp), (int32*)A.getDataPointer());
        break;
    case NLS_SINGLE:
        negate<float>(A.getElementCount(), static_cast<float*>(Cp), (float*)A.getDataPointer());
        break;
    case NLS_DOUBLE:
        negate<double>(A.getElementCount(), static_cast<double*>(Cp), (double*)A.getDataPointer());
        break;
    case NLS_SCOMPLEX:
        negate<float>(2 * A.getElementCount(), static_cast<float*>(Cp), (float*)A.getDataPointer());
        break;
    case NLS_DCOMPLEX:
        negate<double>(
            2 * A.getElementCount(), static_cast<double*>(Cp), (double*)A.getDataPointer());
        break;
    }
    C.setDataPointer(Cp);
    return C;
}
} // namespace Nelson
//=============================================================================
