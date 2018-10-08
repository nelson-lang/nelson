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
#include <algorithm>
#include "StringArrayAddition.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, a.getDimensions().getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, a.getDimensions(), elementsC);
    for (indexType i = 0; i < a.getDimensions().getElementCount(); i++) {
        if (elementsA[i].isCharacterArray() && elementsB[i].isCharacterArray()) {
            std::wstring strA = elementsA[i].getContentAsWideString();
            std::wstring strB = elementsB[i].getContentAsWideString();
            elementsC[i] = ArrayOf::characterArrayConstructor(strA + strB);
        } else {
            elementsC[i] = ArrayOf::emptyConstructor();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
scalar_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    indexType Clen = dimsC.getElementCount();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC
        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);
    for (indexType i = 0; i < Clen; i++) {
        if (elementsA[0].isCharacterArray() && elementsB[i].isCharacterArray()) {
            std::wstring strA = elementsA[0].getContentAsWideString();
            std::wstring strB = elementsB[i].getContentAsWideString();
            elementsC[i] = ArrayOf::characterArrayConstructor(strA + strB);
        } else {
            elementsC[i] = ArrayOf::emptyConstructor();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
matrix_scalar_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    indexType Clen = dimsC.getElementCount();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC
        = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);
    for (indexType i = 0; i < Clen; i++) {
        if (elementsA[i].isCharacterArray() && elementsB[0].isCharacterArray()) {
            std::wstring strA = elementsA[i].getContentAsWideString();
            std::wstring strB = elementsB[0].getContentAsWideString();
            elementsC[i] = ArrayOf::characterArrayConstructor(strA + strB);
        } else {
            elementsC[i] = ArrayOf::emptyConstructor();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
vector_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    Dimensions dimsC = Dimensions(
        std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
    indexType Clen = dimsC.getElementCount();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    indexType m = 0;
    for (indexType i = 0; i < dimsA.getElementCount(); i++) {
        for (indexType j = 0; j < dimsB.getElementCount(); j++) {
            if (elementsA[0].isCharacterArray() && elementsB[j].isCharacterArray()) {
                std::wstring strA = elementsA[i].getContentAsWideString();
                std::wstring strB = elementsB[j].getContentAsWideString();
                elementsC[m] = ArrayOf::characterArrayConstructor(strA + strB);
            } else {
                elementsC[m] = ArrayOf::emptyConstructor();
            }
            m++;
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
vector_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false);
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getDimensions().getRows();

            if (elementsA[q].isCharacterArray() && elementsB[m].isCharacterArray()) {
                std::wstring strA = elementsA[q].getContentAsWideString();
                std::wstring strB = elementsB[m].getContentAsWideString();
                elementsC[m] = ArrayOf::characterArrayConstructor(strA + strB);
            } else {
                elementsC[m] = ArrayOf::emptyConstructor();
            }
        }
        q++;
    }
    return res;
}
//=============================================================================
static ArrayOf
matrix_vector_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false);
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getDimensions().getRows();

            if (elementsA[m].isCharacterArray() && elementsB[q].isCharacterArray()) {
                std::wstring strA = elementsA[m].getContentAsWideString();
                std::wstring strB = elementsB[q].getContentAsWideString();
                elementsC[m] = ArrayOf::characterArrayConstructor(strA + strB);
            } else {
                elementsC[m] = ArrayOf::emptyConstructor();
            }
        }
        q++;
    }
    return res;
}
//=============================================================================
static ArrayOf
vector_column_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.getDimensions();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getDimensions().getColumns();
            if (elementsA[j].isCharacterArray() && elementsB[m].isCharacterArray()) {
                std::wstring strA = elementsA[j].getContentAsWideString();
                std::wstring strB = elementsB[m].getContentAsWideString();
                elementsC[m] = ArrayOf::characterArrayConstructor(strA + strB);
            } else {
                elementsC[m] = ArrayOf::emptyConstructor();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
matrix_vector_column_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.getDimensions();
    ArrayOf* elementsA = (ArrayOf*)a.getDataPointer();
    ArrayOf* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false);
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * b.getDimensions().getColumns();
            if (elementsA[m].isCharacterArray() && elementsB[j].isCharacterArray()) {
                std::wstring strA = elementsA[m].getContentAsWideString();
                std::wstring strB = elementsB[j].getContentAsWideString();
                elementsC[m] = ArrayOf::characterArrayConstructor(strA + strB);
            } else {
                elementsC[m] = ArrayOf::emptyConstructor();
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
stringArray_plus_stringArray(const ArrayOf& a, const ArrayOf& b)
{
    if (a.isScalar() && b.isScalar()) {
        std::wstring strA = a.getContentAsWideString();
        std::wstring strB = b.getContentAsWideString();
        return ArrayOf::stringArrayConstructor(strA + strB);
    }
    Dimensions dimsA = a.getDimensions();
    Dimensions dimsB = b.getDimensions();
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return ArrayOf(b);
            } else {
                return ArrayOf(a);
            }
        } else {
            if (!(SameSizeCheck(dimsA, dimsB))) {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
            }
            return ArrayOf(b);
        }
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_string_addition(a, b);
    } else {
        if (a.isScalar() || b.isScalar()) {
            if (a.isScalar()) {
                return scalar_matrix_string_addition(a, b);
            } else {
                // b.isScalar()
                return matrix_scalar_string_addition(a, b);
            }
        } else {
            if (a.isVector() || b.isVector()) {
                if (a.isRowVector() && b.isColumnVector()) {
                    return vector_string_addition(a, b);
                } else if (a.isColumnVector() && b.isRowVector()) {
                    return vector_string_addition(a, b);
                } else if ((a.isRowVector() && b.isRowVector())
                    || (a.isColumnVector() && b.isColumnVector())) {
                    Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                } else {
                    if (dimsA[1] == dimsB[1]) {
                        if (a.isVector()) {
                            return vector_column_matrix_string_addition(a, b);
                        } else {
                            return matrix_vector_column_string_addition(a, b);
                        }
                    } else if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                        if (a.isVector()) {
                            return vector_matrix_string_addition(a, b);
                        } else {
                            return matrix_vector_string_addition(a, b);
                        }
                    } else {
                        Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
                    }
                }
            } else {
                Error(_W("Size mismatch on arguments to arithmetic operator ") + L"+");
            }
        }
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
