//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstring>
#include "StringCompare.hpp"
#include "StringHelpers.hpp"
#include "ToLower.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
getScalarText(const ArrayOf& value, std::wstring& text);
//=============================================================================
static inline bool
isAscii(const std::wstring& s)
{
    for (wchar_t c : s) {
        if (c > 127) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static inline bool
charRowVectorEqualsRaw(const ArrayOf& element, const charType* refData, indexType refLen)
{
    if (element.elementCount() != refLen || !element.isCharacterArray()) {
        return false;
    }
    if (refLen != 0 && !element.isRowVector()) {
        return false;
    }
    return refLen == 0
        || memcmp(element.dataPointer(), refData, static_cast<size_t>(refLen) * sizeof(charType))
        == 0;
}
//=============================================================================
// len == 0: whole strings; len == STRNCMP_ZERO_LENGTH: compare no character
// at all (always equal); otherwise the first len characters.
static bool
compareString(const std::wstring& A, const std::wstring& B, bool bCaseSensitive, indexType len = 0)
{
    bool bEq = false;
    if (len == STRNCMP_ZERO_LENGTH) {
        return true;
    }
    if (len > 0) {
        std::wstring strA;
        std::wstring strB;
        size_t lengthA = A.size();
        size_t lengthB = B.size();
        if (lengthA > static_cast<size_t>(len)) {
            strA = std::wstring(A.c_str(), len);
        } else {
            strA = A;
        }
        if (lengthB > static_cast<size_t>(len)) {
            strB = std::wstring(B.c_str(), len);
        } else {
            strB = B;
        }
        return compareString(strA, strB, bCaseSensitive);
    }
    if (bCaseSensitive) {
        bEq = (A == B);
    } else if (A.size() != B.size()) {
        bEq = false;
    } else if (isAscii(A) && isAscii(B)) {
        bEq = StringHelpers::iequals(A, B);
    } else {
        // Unicode case folding (e.g. accented letters)
        bEq = ToLower(A) == ToLower(B);
    }
    return bEq;
}
//=============================================================================
static ArrayOf
CompareStringString(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len = 0)
{
    bool bEq = false;
    if (A.isRowVectorCharacterArray() && B.isRowVectorCharacterArray()) {
        bEq = compareString(A.asWideString(), B.asWideString(), bCaseSensitive, len);
    } else {
        wstringVector wA = A.asWideStringVector();
        wstringVector wB = B.asWideStringVector();
        if (wA.size() != wB.size()) {
            bEq = false;
        } else {
            bEq = true;
            for (size_t k = 0; k < wA.size(); ++k) {
                if (wA[k] != wB[k]) {
                    bEq = false;
                    break;
                }
            }
        }
    }
    return ArrayOf::logicalScalar(bEq);
}
//=============================================================================
static ArrayOf
StringCompareSameTypes(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimA = A.dimensions();
    Dimensions dimB = B.dimensions();
    if (dimA.equals(dimB)) {
        size_t Clen = dimA.elementCount();
        logical* Cp = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
        auto* cellA = (ArrayOf*)(A.dataPointer());
        auto* cellB = (ArrayOf*)(B.dataPointer());
        if (bCaseSensitive && len == 0) {
            // exact comparison works on the raw charType buffers: no per-element
            // wide-string conversion or allocation
            for (size_t k = 0; k < Clen; k++) {
                const ArrayOf& elementA = cellA[k];
                const ArrayOf& elementB = cellB[k];
                if (elementA.isRowVectorCharacterArray()) {
                    Cp[k] = static_cast<Nelson::logical>(charRowVectorEqualsRaw(elementB,
                        static_cast<const charType*>(elementA.dataPointer()),
                        elementA.elementCount()));
                } else if (elementA.isCharacterArray() && elementB.isCharacterArray()) {
                    wstringVector s1 = elementA.asWideStringVector();
                    wstringVector s2 = elementB.asWideStringVector();
                    if (s1.size() == s2.size()) {
                        Cp[k] = true;
                    }
                } else {
                    Cp[k] = false;
                }
            }
            return ArrayOf(NLS_LOGICAL, dimA, Cp);
        }
        for (size_t k = 0; k < Clen; k++) {
            ArrayOf elementA = cellA[k];
            ArrayOf elementB = cellB[k];
            if (elementA.isRowVectorCharacterArray() && elementB.isRowVectorCharacterArray()) {
                Cp[k] = static_cast<Nelson::logical>(compareString(
                    elementA.asWideString(), elementB.asWideString(), bCaseSensitive, len));
            } else if (elementA.isCharacterArray() && elementB.isCharacterArray()) {
                wstringVector s1 = elementA.asWideStringVector();
                wstringVector s2 = elementB.asWideStringVector();
                if (s1.size() == s2.size()) {
                    Cp[k] = true;
                }
            } else {
                Cp[k] = false;
            }
        }
        return ArrayOf(NLS_LOGICAL, dimA, Cp);
    } else {
        if (dimA.isScalar() || dimB.isScalar()) {
            size_t Clen = 0;
            Dimensions dimC;
            if (dimA.isScalar()) {
                Clen = dimB.elementCount();
                dimC = dimB;
            } else {
                Clen = dimA.elementCount();
                dimC = dimA;
            }
            logical* Cp = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
            auto* cellA = (ArrayOf*)A.dataPointer();
            auto* cellB = (ArrayOf*)B.dataPointer();
            for (size_t k = 0; k < Clen; ++k) {
                ArrayOf p1;
                ArrayOf p2;
                if (dimA.isScalar()) {
                    p1 = cellA[0];
                    p2 = cellB[k];
                } else {
                    p1 = cellA[k];
                    p2 = cellB[0];
                }
                if (p1.isCharacterArray() && p2.isCharacterArray()) {
                    wstringVector s1 = p1.asWideStringVector();
                    wstringVector s2 = p2.asWideStringVector();
                    if (s1.size() == s2.size()) {
                        Cp[k] = true;
                        for (size_t l = 0; l < s1.size(); ++l) {
                            if (!compareString(s1[l], s2[l], bCaseSensitive, len)) {
                                Cp[k] = false;
                                break;
                            }
                        }
                    }
                }
            }
            return ArrayOf(NLS_LOGICAL, dimC, Cp);
        } else {
            Error(L"Nelson:strcmp:InputsSizeMismatch",
                L"Inputs must be the same size or either one can be a scalar.");
        }
    }
    return {};
}
//=============================================================================
static ArrayOf
StringCompareMixedTypes(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if ((A.isCell() && A.isScalar() && B.isScalarStringArray())
        || (B.isCell() && B.isScalar() && A.isScalarStringArray())) {
        std::wstring scalarStr;
        ArrayOf* elements;
        Dimensions dims;
        if (A.isScalarStringArray()) {
            scalarStr = A.asWideString();
            elements = (ArrayOf*)B.dataPointer();
            dims = B.dimensions();
        } else {
            scalarStr = B.asWideString();
            elements = (ArrayOf*)A.dataPointer();
            dims = A.dimensions();
        }
        std::wstring cellScalarStr = elements[0].asWideString();
        return ArrayOf::logicalScalar(compareString(scalarStr, cellScalarStr, bCaseSensitive));
    }

    if ((A.isCell() && B.isStringArray()) || ((B.isCell() && A.isStringArray()))) {
        if (dimsA.equals(dimsB)) {
            auto elementsA = (ArrayOf*)A.dataPointer();
            auto elementsB = (ArrayOf*)B.dataPointer();
            logical* Cp = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, dimsA.elementCount(), stringVector(), true));
            for (indexType k = 0; k < dimsA.elementCount(); ++k) {
                if (elementsA[k].isCharacterArray() && elementsB[k].isCharacterArray()) {
                    Cp[k] = compareString(elementsA[k].asWideString(), elementsB[k].asWideString(),
                        bCaseSensitive, len);
                } else {
                    Cp[k] = false;
                }
            }
            return ArrayOf(NLS_LOGICAL, dimsA, Cp);
        }
    }

    bool checkDims = false;
    if ((!A.isCell() && !A.isStringArray()) || (!B.isCell() && !B.isStringArray())) {
        checkDims = true;
    } else {
        checkDims = A.isRowVectorCharacterArray() || B.isRowVectorCharacterArray()
            || (A.isStringArray() && A.isScalar()) || (B.isStringArray() && B.isScalar())
            || (A.isCell() && A.isScalar()) || (B.isCell() && B.isScalar()) || dimsA.equals(dimsB);
    }
    if (!checkDims) {
        Error(L"Nelson:strcmp:InputsSizeMismatch",
            L"Inputs must be the same size or either one can be a scalar.");
    }
    // One operand supplies a single text value compared against every element of the other
    // (a cell or string array). The iterated list is the cell/string array that is not a
    // scalar text, so a scalar string or scalar cell stays on the scalar side and
    // broadcasts across the list.
    std::wstring ignored;
    bool aIsList = (A.isCell() || A.isStringArray()) && !getScalarText(A, ignored);
    bool bIsList = (B.isCell() || B.isStringArray()) && !getScalarText(B, ignored);
    ArrayOf listArray;
    ArrayOf scalarArg;
    if (aIsList) {
        listArray = A;
        scalarArg = B;
    } else if (bIsList) {
        listArray = B;
        scalarArg = A;
    } else if (A.isCell() || A.isStringArray()) {
        listArray = A;
        scalarArg = B;
    } else {
        listArray = B;
        scalarArg = A;
    }
    Dimensions dimC = listArray.dimensions();
    size_t Clen = dimC.elementCount();

    logical* Cp
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
    auto* cellA = (ArrayOf*)(listArray.dataPointer());
    std::wstring scalarWide;
    if (!getScalarText(scalarArg, scalarWide)) {
        // Cp is zero-initialized: nothing compares equal to a non-text scalar
        return ArrayOf(NLS_LOGICAL, dimC, Cp);
    }
    if (bCaseSensitive && len == 0 && scalarArg.isRowVectorCharacterArray()) {
        // exact comparison works on the raw charType buffers: no per-element
        // wide-string conversion of the elements nor of the scalar
        const auto* refData = static_cast<const charType*>(scalarArg.dataPointer());
        const indexType refLen = scalarArg.elementCount();
        for (size_t k = 0; k < Clen; k++) {
            Cp[k] = static_cast<Nelson::logical>(
                cellA[k].isCharacterArray() && charRowVectorEqualsRaw(cellA[k], refData, refLen));
        }
        return ArrayOf(NLS_LOGICAL, dimC, Cp);
    }
    for (size_t k = 0; k < Clen; k++) {
        ArrayOf elementA = cellA[k];
        if (elementA.isCharacterArray()) {
            Cp[k] = static_cast<Nelson::logical>(
                compareString(elementA.asWideString(), scalarWide, bCaseSensitive, len));
        } else {
            Cp[k] = false;
        }
    }
    return ArrayOf(NLS_LOGICAL, dimC, Cp);
}
//=============================================================================
static ArrayOf
StringCompareEmpty(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimA = A.dimensions();
    Dimensions dimB = B.dimensions();

    Dimensions dim;
    if (dimA.equals(dimB)) {
        dim = dimA;
    } else if (dimA.isSquare()) {
        dim = dimB;
    } else if (dimB.isSquare()) {
        dim = dimA;
    } else {
        Error(L"Nelson:strcmp:InputsSizeMismatch",
            L"Inputs must be the same size or either one can be a scalar.");
    }

    charType* Cp
        = static_cast<charType*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, 0, stringVector(), true));
    return ArrayOf(NLS_LOGICAL, dim, Cp);
}
//=============================================================================
ArrayOf
StringCompare(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    if (A.isEmpty() && B.isEmpty() && (!A.isCharacterArray() && !B.isCharacterArray())) {
        return StringCompareEmpty(A, B, bCaseSensitive, len);
    }
    if (A.isCharacterArray() && B.isCharacterArray()) {
        return CompareStringString(A, B, bCaseSensitive, len);
    }
    if ((A.isCell() && A.isEmpty()) || (B.isCell() && B.isEmpty())
        || (A.isStringArray() && A.isEmpty()) || (B.isStringArray() && B.isEmpty())) {
        return ArrayOf::empty();
    }
    if ((A.isCell() && B.isCell()) || (A.isStringArray() && B.isStringArray())) {
        return StringCompareSameTypes(A, B, bCaseSensitive, len);
    }
    if (A.isCell() || B.isCell() || A.isStringArray() || B.isStringArray()) {
        return StringCompareMixedTypes(A, B, bCaseSensitive, len);
    }
    return ArrayOf::logicalScalar(false);
}
//=============================================================================
static bool
getScalarText(const ArrayOf& value, std::wstring& text)
{
    if (value.isCharacterArray()) {
        text = value.asWideString();
        return true;
    }
    if ((value.isStringArray() || value.isCell()) && value.isScalar()) {
        const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
        if (elements[0].isCharacterArray()) {
            text = elements[0].asWideString();
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
