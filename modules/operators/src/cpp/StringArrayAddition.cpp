//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "StringArrayAddition.hpp"
#include "MatrixCheck.hpp"
#include "Exception.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, a.getElementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, a.getDimensions(), elementsC);
    indexType elementCount = a.getElementCount();
    for (indexType i = 0; i < elementCount; i++) {
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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false));
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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false));
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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    indexType m = 0;
    indexType elementCountA = dimsA.getElementCount();
    indexType elementCountB = dimsB.getElementCount();
    for (indexType i = 0; i < elementCountA; i++) {
        for (indexType j = 0; j < elementCountB; j++) {
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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false));
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getRows();

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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false));
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getRows();

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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * a.getColumns();
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
    auto* elementsA = (ArrayOf*)a.getDataPointer();
    auto* elementsB = (ArrayOf*)b.getDataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.getElementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.getRows(); i++) {
        for (indexType j = 0; j < dimsC.getColumns(); j++) {
            indexType m = i + j * b.getColumns();
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
            }
            return ArrayOf(a);
        }
        if (!(SameSizeCheck(dimsA, dimsB))) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
        }
        return ArrayOf(b);
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_string_addition(a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return scalar_matrix_string_addition(a, b);
        }
        // b.isScalar()
        return matrix_scalar_string_addition(a, b);
    }
    if (a.isVector() || b.isVector()) {
        if (a.isRowVector() && b.isColumnVector()) {
            return vector_string_addition(a, b);
        }
        if (a.isColumnVector() && b.isRowVector()) {
            return vector_string_addition(a, b);
        }
        if ((a.isRowVector() && b.isRowVector()) || (a.isColumnVector() && b.isColumnVector())) {
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
        } else {
            if (dimsA[1] == dimsB[1]) {
                if (a.isVector()) {
                    return vector_column_matrix_string_addition(a, b);
                }
                return matrix_vector_column_string_addition(a, b);
            }
            if ((dimsA[0] == dimsB[0]) && (dimsA[0] != 1)) {
                if (a.isVector()) {
                    return vector_matrix_string_addition(a, b);
                }
                return matrix_vector_string_addition(a, b);
            }
            Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
        }
    } else {
        Error(_("Size mismatch on arguments to arithmetic operator") + " " + "+");
    }

    return ArrayOf::emptyConstructor();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
