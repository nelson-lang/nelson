//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringHelpers.hpp"
#include "StringMatches.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
matchesString(const std::wstring& A, const std::wstring& B, bool ignoreCase)
{
    if (!ignoreCase) {
        return (A == B);
    }
    return StringHelpers::iequals(A, B);
}
//=============================================================================
ArrayOf
StringMatches(ArrayOf A, ArrayOf B, bool ignoreCase)
{
    ArrayOf res;
    bool isSupportedA
        = (A.isRowVectorCharacterArray() || A.isStringArray() || A.isCellArrayOfCharacterVectors());
    if (!isSupportedA) {
        Error(_W("Wrong type for argument #1: string array or character vector or cell array of "
                 "character vectors expected."));
    }
    bool isSupportedB
        = (B.isRowVectorCharacterArray() || B.isStringArray() || B.isCellArrayOfCharacterVectors());
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
