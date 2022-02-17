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
#include <boost/algorithm/string.hpp>
#include "StringMatches.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
matchesString(const std::wstring& A, const std::wstring& B, bool ignoreCase)
{
    if (!ignoreCase) {
        return (A == B);
    }
    return boost::iequals(A, B);

    return false;
}
//=============================================================================
ArrayOf
StringMatches(ArrayOf A, ArrayOf B, bool ignoreCase)
{
    ArrayOf res;
    bool isSupportedA = (A.isRowVectorCharacterArray() || A.isStringArray() || IsCellOfString(A));
    if (!isSupportedA) {
        Error(_W("Wrong type for argument #1: string array or character vector or cell array of "
                 "character vectors expected."));
    }
    bool isSupportedB = (B.isRowVectorCharacterArray() || B.isStringArray() || IsCellOfString(B));
    if (!isSupportedB) {
        Error(_W("Wrong type for argument #2: string array or character vector or cell array of "
                 "character vectors expected."));
    }
    if ((A.isCell() && A.isEmpty()) || (B.isCell() && B.isEmpty())
        || (A.isStringArray() && A.isEmpty()) || (B.isStringArray() && B.isEmpty())) {
        return ArrayOf::emptyConstructor();
    }
    if (A.getDataClass() == NLS_CHAR) {
        std::wstring str = A.getContentAsWideString();
        A = ArrayOf::stringArrayConstructor(str);
    }
    Dimensions dimsA = A.getDimensions();
    if (B.getDataClass() == NLS_CHAR) {
        std::wstring str = B.getContentAsWideString();
        B = ArrayOf::stringArrayConstructor(str);
    }
    Dimensions dimsB = B.getDimensions();
    logical* pRes = (logical*)ArrayOf::allocateArrayOf(
        NLS_LOGICAL, dimsA.getElementCount(), stringVector(), true);
    res = ArrayOf(NLS_LOGICAL, dimsA, pRes);
    ArrayOf* elementsA = (ArrayOf*)A.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)B.getDataPointer();
    indexType nbElementsA = dimsA.getElementCount();
    indexType nbElementsB = dimsB.getElementCount();
    indexType pos = 0;
    for (indexType idxA = 0; idxA < nbElementsA; idxA++) {
        ArrayOf elementA = elementsA[idxA];
        if (elementA.isCharacterArray()) {
            std::wstring strA = elementA.getContentAsWideString();
            for (indexType idxB = 0; idxB < nbElementsB; idxB++) {
                ArrayOf elementB = elementsB[idxB];
                if (elementB.isCharacterArray()) {
                    std::wstring strB = elementB.getContentAsWideString();
                    if (matchesString(strA, strB, ignoreCase)) {
                        pRes[pos] = logical(1);
                        break;
                    }
                }
            }
        }
        pos++;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
