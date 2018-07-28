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
#include "StringIsEqual.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
StringIsEqual(ArrayOf A, ArrayOf B)
{
    bool bRes = false;
    if (A.isCharacterArray() && B.isCharacterArray()) {
        if (A.isNdArrayCharacterType() && B.isNdArrayCharacterType()) {
            if (!A.getDimensions().equals(B.getDimensions())) {
                return false;
            }
            A.promoteType(NLS_DOUBLE);
            B.promoteType(NLS_DOUBLE);
            double* ptrA = (double*)A.getDataPointer();
            double* ptrB = (double*)B.getDataPointer();
            for (indexType k = 0; k < A.getDimensions().getElementCount(); k++) {
                if (ptrA[k] != ptrB[k]) {
                    return false;
                }
            }
            return true;
        } else {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            if (dimsA.equals(dimsB)) {
                wstringVector valueA = A.getContentAsWideStringVector();
                wstringVector valueB = B.getContentAsWideStringVector();
                if (valueA.size() != valueB.size()) {
                    return false;
                }
                for (size_t k = 0; k < valueA.size(); ++k) {
                    std::wstring a = valueA[k];
                    std::wstring b = valueB[k];
                    if (a != b) {
                        return false;
                    }
                }
                return true;
            }
        }
    }
    return bRes;
}
//=============================================================================
}
//=============================================================================
