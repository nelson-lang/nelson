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
#include "StringCompare.hpp"
#include <boost/algorithm/string.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
compareString(std::wstring A, std::wstring B, bool bCaseSensitive, indexType len = 0)
{
    bool bEq = false;
    if (len > 0) {
        std::wstring strA;
        std::wstring strB;
        size_t lenghtA = A.size();
        size_t lenghtB = B.size();
        if (lenghtA > len) {
            strA = std::wstring(A.c_str(), len);
        } else {
            strA = A;
        }
        if (lenghtB > len) {
            strB = std::wstring(B.c_str(), len);
        } else {
            strB = B;
        }
        return compareString(strA, strB, 0);
    } else {
        if (bCaseSensitive) {
            bEq = (A.compare(B) == 0);
        } else {
            bEq = boost::iequals(A, B);
        }
    }
    return bEq;
}
//=============================================================================
static ArrayOf
CompareStringString(ArrayOf A, ArrayOf B, bool bCaseSensitive, indexType len = 0)
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
ArrayOf
StringCompare(ArrayOf A, ArrayOf B, bool bCaseSensitive, indexType len)
{
    ArrayOf res;
    if (A.isCharacterArray() && B.isCharacterArray()) {
        return CompareStringString(A, B, bCaseSensitive, len);
    }
    if ((A.isCell() && A.isEmpty()) || (B.isCell() && B.isEmpty())
        || (A.isStringArray() && A.isEmpty()) || (B.isStringArray() && B.isEmpty())) {
        return ArrayOf::emptyConstructor();
    } else {
        if ((A.isCell() && B.isCell()) || (A.isStringArray() && B.isStringArray())) {
            Dimensions dimA = A.getDimensions();
            Dimensions dimB = B.getDimensions();
            if (dimA.equals(dimB)) {
                size_t Clen = dimA.getElementCount();
                logical* Cp = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen);
                ArrayOf* cellA = (ArrayOf*)(A.getDataPointer());
                ArrayOf* cellB = (ArrayOf*)(B.getDataPointer());
                for (size_t k = 0; k < Clen; k++) {
                    ArrayOf elementA = cellA[k];
                    ArrayOf elementB = cellB[k];
                    if (elementA.isRowVectorCharacterArray()
                        && elementB.isRowVectorCharacterArray()) {
                        Cp[k] = compareString(elementA.getContentAsWideString(),
                            elementB.getContentAsWideString(), bCaseSensitive, len);
                    } else if (elementA.isCharacterArray() && elementB.isCharacterArray()) {
                        wstringVector s1 = elementA.getContentAsWideStringVector();
                        wstringVector s2 = elementB.getContentAsWideStringVector();
                        if (s1.size() == s2.size()) {
                            for (size_t l = 0; l < s1.size(); ++l) {
                                if (s1[l] != s2[l]) {
                                    Cp[k] = false;
                                    break;
                                }
                            }
                            Cp[k] = true;
                        }
                    } else {
                        Cp[k] = false;
                    }
                }
                res = ArrayOf(NLS_LOGICAL, dimA, Cp);
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
                    logical* Cp = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen);
                    ArrayOf* cellA = (ArrayOf*)A.getDataPointer();
                    ArrayOf* cellB = (ArrayOf*)B.getDataPointer();
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
                    res = ArrayOf(NLS_LOGICAL, dimC, Cp);
                } else {
                    Error(ERROR_SAME_SIZE_EXPECTED);
                }
            }
        } else if (A.isCell() || B.isCell() || A.isStringArray() || B.isStringArray()) {
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();

            bool checkDims = false;
            if ((!A.isCell() && !A.isStringArray()) || (!B.isCell() && !B.isStringArray())) {
                checkDims = true;
            } else {
                checkDims = A.isRowVectorCharacterArray() || B.isRowVectorCharacterArray()
                    || (A.isStringArray() && A.isScalar()) || (B.isStringArray() && B.isScalar())
                    || (A.isCell() && A.isScalar()) || (B.isCell() && B.isScalar())
                    || dimsA.equals(dimsB);
            }
            if (!checkDims) {
                Error(_W("Same size or scalar expected."));
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

            logical* Cp = (logical*)ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen);
            ArrayOf* cellA = (ArrayOf*)(cell1.getDataPointer());
            for (size_t k = 0; k < Clen; k++) {
                if (!scalar2.isCharacterArray()) {
                    Cp[k] = false;
                } else {
                    ArrayOf elementA = cellA[k];
                    if (elementA.isCharacterArray() && scalar2.isCharacterArray()) {
                        Cp[k] = compareString(elementA.getContentAsWideString(),
                            scalar2.getContentAsWideString(), bCaseSensitive, len);
                    } else {
                        Cp[k] = false;
                    }
                }
            }
            res = ArrayOf(NLS_LOGICAL, dimC, Cp);
        } else {
            res = ArrayOf::logicalConstructor(false);
        }
    }
    return res;
}
//=============================================================================
}
//=============================================================================
