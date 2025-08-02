//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringCompare.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
compareString(const std::wstring& A, const std::wstring& B, bool bCaseSensitive, indexType len = 0)
{
    bool bEq = false;
    if (len > 0) {
        std::wstring strA;
        std::wstring strB;
        size_t lenghtA = A.size();
        size_t lenghtB = B.size();
        if (lenghtA > static_cast<size_t>(len)) {
            strA = std::wstring(A.c_str(), len);
        } else {
            strA = A;
        }
        if (lenghtB > static_cast<size_t>(len)) {
            strB = std::wstring(B.c_str(), len);
        } else {
            strB = B;
        }
        return compareString(strA, strB, false);
    }
    if (bCaseSensitive) {
        bEq = (A == B);
    } else {
        bEq = StringHelpers::iequals(A, B);
    }
    return bEq;
}
//=============================================================================
static ArrayOf
CompareStringString(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len = 0)
{
    bool bEq = false;
    if (A.isRowVectorCharacterArray() && B.isRowVectorCharacterArray()) {
        bEq = compareString(
            A.getContentAsWideString(), B.getContentAsWideString(), bCaseSensitive, len);
    } else {
        wstringVector wA = A.getContentAsWideStringVector();
        wstringVector wB = B.getContentAsWideStringVector();
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
    return ArrayOf::logicalConstructor(bEq);
}
//=============================================================================
static ArrayOf
StringCompareSameTypes(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimA = A.getDimensions();
    Dimensions dimB = B.getDimensions();
    if (dimA.equals(dimB)) {
        size_t Clen = dimA.getElementCount();
        logical* Cp = static_cast<logical*>(
            ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
        auto* cellA = (ArrayOf*)(A.getDataPointer());
        auto* cellB = (ArrayOf*)(B.getDataPointer());
        for (size_t k = 0; k < Clen; k++) {
            ArrayOf elementA = cellA[k];
            ArrayOf elementB = cellB[k];
            if (elementA.isRowVectorCharacterArray() && elementB.isRowVectorCharacterArray()) {
                Cp[k]
                    = static_cast<Nelson::logical>(compareString(elementA.getContentAsWideString(),
                        elementB.getContentAsWideString(), bCaseSensitive, len));
            } else if (elementA.isCharacterArray() && elementB.isCharacterArray()) {
                wstringVector s1 = elementA.getContentAsWideStringVector();
                wstringVector s2 = elementB.getContentAsWideStringVector();
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
                Clen = dimB.getElementCount();
                dimC = dimB;
            } else {
                Clen = dimA.getElementCount();
                dimC = dimA;
            }
            logical* Cp = static_cast<logical*>(
                ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
            auto* cellA = (ArrayOf*)A.getDataPointer();
            auto* cellB = (ArrayOf*)B.getDataPointer();
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
                    wstringVector s1 = p1.getContentAsWideStringVector();
                    wstringVector s2 = p2.getContentAsWideStringVector();
                    if (s1.size() == s2.size()) {
                        Cp[k] = true;
                        for (size_t l = 0; l < s1.size(); ++l) {
                            if (s1[l] != s2[l]) {
                                Cp[k] = false;
                                break;
                            }
                        }
                    }
                }
            }
            return ArrayOf(NLS_LOGICAL, dimC, Cp);
        } else {
            Error(_W("Inputs must be the same size or either one can be a scalar."),
                L"Nelson:strcmp:InputsSizeMismatch");
        }
    }
    return {};
}
//=============================================================================
static ArrayOf
StringCompareMixedTypes(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if ((A.isCell() && A.isScalar() && B.isScalarStringArray())
        || (B.isCell() && B.isScalar() && A.isScalarStringArray())) {
        std::wstring scalarStr;
        ArrayOf* elements;
        Dimensions dims;
        if (A.isScalarStringArray()) {
            scalarStr = A.getContentAsWideString();
            elements = (ArrayOf*)B.getDataPointer();
            dims = B.getDimensions();
        } else {
            scalarStr = B.getContentAsWideString();
            elements = (ArrayOf*)A.getDataPointer();
            dims = A.getDimensions();
        }
        std::wstring cellScalarStr = elements[0].getContentAsWideString();
        return ArrayOf::logicalConstructor(compareString(scalarStr, cellScalarStr, bCaseSensitive));
    }

    if ((A.isCell() && B.isStringArray()) || ((B.isCell() && A.isStringArray()))) {
        if (dimsA.equals(dimsB)) {
            auto elementsA = (ArrayOf*)A.getDataPointer();
            auto elementsB = (ArrayOf*)B.getDataPointer();
            logical* Cp = static_cast<logical*>(ArrayOf::allocateArrayOf(
                NLS_LOGICAL, dimsA.getElementCount(), stringVector(), true));
            for (indexType k = 0; k < dimsA.getElementCount(); ++k) {
                if (elementsA[k].isCharacterArray() && elementsB[k].isCharacterArray()) {
                    Cp[k] = compareString(elementsA[k].getContentAsWideString(),
                        elementsB[k].getContentAsWideString(), bCaseSensitive, len);
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
        Error(_W("Inputs must be the same size or either one can be a scalar."),
            L"Nelson:strcmp:InputsSizeMismatch");
    }
    size_t Clen;
    Dimensions dimC;
    ArrayOf cell1;
    ArrayOf scalar2;
    if (A.isCell() || A.isStringArray()) {
        cell1 = A;
        scalar2 = B;
        dimC = A.getDimensions();
        Clen = dimC.getElementCount();
    } else {
        cell1 = B;
        scalar2 = A;
        dimC = B.getDimensions();
        Clen = dimC.getElementCount();
    }

    logical* Cp
        = static_cast<logical*>(ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true));
    auto* cellA = (ArrayOf*)(cell1.getDataPointer());
    for (size_t k = 0; k < Clen; k++) {
        if (!scalar2.isCharacterArray() && !scalar2.isScalarStringArray()) {
            Cp[k] = false;
        } else {
            ArrayOf elementA = cellA[k];
            if ((elementA.isCharacterArray()) && (scalar2.isCharacterArray())) {
                Cp[k]
                    = static_cast<Nelson::logical>(compareString(elementA.getContentAsWideString(),
                        scalar2.getContentAsWideString(), bCaseSensitive, len));
            } else {
                Cp[k] = false;
            }
        }
    }
    return ArrayOf(NLS_LOGICAL, dimC, Cp);
}
//=============================================================================
static ArrayOf
StringCompareEmpty(const ArrayOf& A, const ArrayOf& B, bool bCaseSensitive, indexType len)
{
    Dimensions dimA = A.getDimensions();
    Dimensions dimB = B.getDimensions();

    Dimensions dim;
    if (dimA.equals(dimB)) {
        dim = dimA;
    } else if (dimA.isSquare()) {
        dim = dimB;
    } else if (dimB.isSquare()) {
        dim = dimA;
    } else {
        Error(_W("Inputs must be the same size or either one can be a scalar."),
            L"Nelson:strcmp:InputsSizeMismatch");
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
        return ArrayOf::emptyConstructor();
    }
    if ((A.isCell() && B.isCell()) || (A.isStringArray() && B.isStringArray())) {
        return StringCompareSameTypes(A, B, bCaseSensitive, len);
    }
    if (A.isCell() || B.isCell() || A.isStringArray() || B.isStringArray()) {
        return StringCompareMixedTypes(A, B, bCaseSensitive, len);
    }
    return ArrayOf::logicalConstructor(false);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
