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
#include "IntegerIsEqual.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
bool
IntegerIsEqualTemplate(ArrayOf A, ArrayOf B)
{
    T* ptrA = (T*)A.getDataPointer();
    T* ptrB = (T*)B.getDataPointer();
    for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
        if (ptrA[k] != ptrB[k]) {
            return false;
        }
    }
    return true;
}
//=============================================================================
bool
IntegerIsEqual(ArrayOf A, ArrayOf B)
{
    if (A.getDataClass() == B.getDataClass()) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (dimsA.equals(dimsB)) {
            switch (A.getDataClass()) {
            case NLS_INT8:
                return IntegerIsEqualTemplate<int8>(A, B);
            case NLS_INT16:
                return IntegerIsEqualTemplate<int16>(A, B);
            case NLS_INT32:
                return IntegerIsEqualTemplate<int32>(A, B);
            case NLS_INT64:
                return IntegerIsEqualTemplate<int64>(A, B);
            case NLS_UINT8:
                return IntegerIsEqualTemplate<uint8>(A, B);
            case NLS_UINT16:
                return IntegerIsEqualTemplate<uint16>(A, B);
            case NLS_UINT32:
                return IntegerIsEqualTemplate<uint32>(A, B);
            case NLS_UINT64:
                return IntegerIsEqualTemplate<uint64>(A, B);
            }
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
