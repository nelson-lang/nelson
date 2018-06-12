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
#include "SingleIsEqual.hpp"
#include "ImagPart.hpp"
#include "RealPart.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
single_isequal(ArrayOf a, ArrayOf b)
{
    if ((a.getDataClass() == NLS_SCOMPLEX || a.getDataClass() == NLS_SINGLE)
        && (b.getDataClass() == NLS_SCOMPLEX || b.getDataClass() == NLS_SINGLE)) {
        Dimensions dimsA = a.getDimensions();
        Dimensions dimsB = b.getDimensions();
        if (dimsA.equals(dimsB)) {
            if (a.getDataClass() == NLS_SINGLE && b.getDataClass() == NLS_SINGLE) {
                single* ptrA = (single*)a.getDataPointer();
                single* ptrB = (single*)b.getDataPointer();
                for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                    if (ptrA[k] != ptrB[k]) {
                        return false;
                    }
                }
            } else {
                ArrayOf realPartA = RealPart(a);
                ArrayOf realPartB = RealPart(b);
                ArrayOf imagPartA = ImagPart(a);
                ArrayOf imagPartB = ImagPart(b);
                single* ptrRealA = (single*)realPartA.getDataPointer();
                single* ptrRealB = (single*)realPartB.getDataPointer();
                single* ptrImagA = (single*)imagPartA.getDataPointer();
                single* ptrImagB = (single*)imagPartB.getDataPointer();
                for (indexType k = 0; k < dimsA.getElementCount(); k++) {
                    if ((ptrRealA[k] != ptrRealB[k]) || (ptrImagA[k] != ptrImagB[k])) {
                        return false;
                    }
                }
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
