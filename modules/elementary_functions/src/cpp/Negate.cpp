//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Negate.hpp"
//=============================================================================
namespace Nelson {
template <class T>
void
negate(indexType N, T* C, const T* A)
{
    for (indexType i = 0; i < N; i++) {
        C[i] = -A[i];
    }
}

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
    C = ArrayOf(Aclass, A.getDimensions(), NULL);
    void* Cp = C.allocateArrayOf(Aclass, A.getLength());
    switch (Aclass) {
    case NLS_INT32:
        negate<int32>(A.getLength(), (int32*)Cp, (int32*)A.getDataPointer());
        break;
    case NLS_SINGLE:
        negate<float>(A.getLength(), (float*)Cp, (float*)A.getDataPointer());
        break;
    case NLS_DOUBLE:
        negate<double>(A.getLength(), (double*)Cp, (double*)A.getDataPointer());
        break;
    case NLS_SCOMPLEX:
        negate<float>(2 * A.getLength(), (float*)Cp, (float*)A.getDataPointer());
        break;
    case NLS_DCOMPLEX:
        negate<double>(2 * A.getLength(), (double*)Cp, (double*)A.getDataPointer());
        break;
    }
    C.setDataPointer(Cp);
    return C;
}
}
//=============================================================================
