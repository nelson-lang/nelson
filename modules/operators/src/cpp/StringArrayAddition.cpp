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
#include "BinaryOperatorsHelpers.hpp"
#include "MatrixCheck.hpp"
#include "NelsonException.hpp"
#include "i18n.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
matrix_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, a.elementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, a.dimensions(), elementsC);
    indexType elementCount = a.elementCount();
    for (indexType i = 0; i < elementCount; i++) {
        if (elementsA[i].isCharacterArray() && elementsB[i].isCharacterArray()) {
            std::wstring strA = elementsA[i].asWideString();
            std::wstring strB = elementsB[i].asWideString();
            elementsC[i] = ArrayOf::characterArray(strA + strB);
        } else {
            elementsC[i] = ArrayOf::empty();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
scalar_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.dimensions();
    indexType Clen = dimsC.elementCount();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);
    for (indexType i = 0; i < Clen; i++) {
        if (elementsA[0].isCharacterArray() && elementsB[i].isCharacterArray()) {
            std::wstring strA = elementsA[0].asWideString();
            std::wstring strB = elementsB[i].asWideString();
            elementsC[i] = ArrayOf::characterArray(strA + strB);
        } else {
            elementsC[i] = ArrayOf::empty();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
matrix_scalar_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.dimensions();
    indexType Clen = dimsC.elementCount();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);
    for (indexType i = 0; i < Clen; i++) {
        if (elementsA[i].isCharacterArray() && elementsB[0].isCharacterArray()) {
            std::wstring strA = elementsA[i].asWideString();
            std::wstring strB = elementsB[0].asWideString();
            elementsC[i] = ArrayOf::characterArray(strA + strB);
        } else {
            elementsC[i] = ArrayOf::empty();
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
broadcast_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    BinaryBroadcast info = binaryOperatorBroadcast(a, b, "+");
    indexType Clen = info.dimensions.elementCount();
    const auto* elementsA = static_cast<const ArrayOf*>(a.dataPointer());
    const auto* elementsB = static_cast<const ArrayOf*>(b.dataPointer());
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, Clen, stringVector(), false));
    if (a.elementCount() > 1 && b.elementCount() > 1 && a.elementCount() != b.elementCount()) {
        indexType rows = info.dimensions.rows();
        indexType columns = info.dimensions.columns();
        bool aIsColumn = a.elementCount() == rows && b.elementCount() == columns;
        bool bIsColumn = b.elementCount() == rows && a.elementCount() == columns;
        if (aIsColumn || bIsColumn) {
            for (indexType col = 0; col < columns; ++col) {
                for (indexType row = 0; row < rows; ++row) {
                    indexType indexA = aIsColumn ? row : col;
                    indexType indexB = aIsColumn ? col : row;
                    indexType indexC = row + col * rows;
                    if (elementsA[indexA].isCharacterArray()
                        && elementsB[indexB].isCharacterArray()) {
                        std::wstring strA = elementsA[indexA].asWideString();
                        std::wstring strB = elementsB[indexB].asWideString();
                        elementsC[indexC] = ArrayOf::characterArray(strA + strB);
                    } else {
                        elementsC[indexC] = ArrayOf::empty();
                    }
                }
            }
            return ArrayOf(NLS_STRING_ARRAY, info.dimensions, elementsC);
        }
    }
    for (indexType i = 0; i < Clen; ++i) {
        indexType indexA = binaryOperatorBroadcastIndex(info, i, info.aStrides);
        indexType indexB = binaryOperatorBroadcastIndex(info, i, info.bStrides);
        if (elementsA[indexA].isCharacterArray() && elementsB[indexB].isCharacterArray()) {
            std::wstring strA = elementsA[indexA].asWideString();
            std::wstring strB = elementsB[indexB].asWideString();
            elementsC[i] = ArrayOf::characterArray(strA + strB);
        } else {
            elementsC[i] = ArrayOf::empty();
        }
    }
    return ArrayOf(NLS_STRING_ARRAY, info.dimensions, elementsC);
}
//=============================================================================
static ArrayOf
vector_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    bool aIsColumn = a.columns() == 1 && a.rows() != 1;
    indexType rows = aIsColumn ? a.elementCount() : b.elementCount();
    indexType columns = aIsColumn ? b.elementCount() : a.elementCount();
    Dimensions dimsC = Dimensions(rows, columns);
    indexType Clen = dimsC.elementCount();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.elementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType col = 0; col < columns; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            indexType indexA = aIsColumn ? row : col;
            indexType indexB = aIsColumn ? col : row;
            indexType indexC = row + col * rows;
            if (elementsA[indexA].isCharacterArray() && elementsB[indexB].isCharacterArray()) {
                std::wstring strA = elementsA[indexA].asWideString();
                std::wstring strB = elementsB[indexB].asWideString();
                elementsC[indexC] = ArrayOf::characterArray(strA + strB);
            } else {
                elementsC[indexC] = ArrayOf::empty();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
vector_matrix_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = b.dimensions();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.elementCount(), stringVector(), false));
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * a.rows();

            if (elementsA[q].isCharacterArray() && elementsB[m].isCharacterArray()) {
                std::wstring strA = elementsA[q].asWideString();
                std::wstring strB = elementsB[m].asWideString();
                elementsC[m] = ArrayOf::characterArray(strA + strB);
            } else {
                elementsC[m] = ArrayOf::empty();
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
    Dimensions dimsC = a.dimensions();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.elementCount(), stringVector(), false));
    indexType q = 0;
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * a.rows();

            if (elementsA[m].isCharacterArray() && elementsB[q].isCharacterArray()) {
                std::wstring strA = elementsA[m].asWideString();
                std::wstring strB = elementsB[q].asWideString();
                elementsC[m] = ArrayOf::characterArray(strA + strB);
            } else {
                elementsC[m] = ArrayOf::empty();
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
    Dimensions dimsC = b.dimensions();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.elementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * a.columns();
            if (elementsA[j].isCharacterArray() && elementsB[m].isCharacterArray()) {
                std::wstring strA = elementsA[j].asWideString();
                std::wstring strB = elementsB[m].asWideString();
                elementsC[m] = ArrayOf::characterArray(strA + strB);
            } else {
                elementsC[m] = ArrayOf::empty();
            }
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
matrix_vector_column_string_addition(const ArrayOf& a, const ArrayOf& b)
{
    Dimensions dimsC = a.dimensions();
    auto* elementsA = (ArrayOf*)a.dataPointer();
    auto* elementsB = (ArrayOf*)b.dataPointer();
    ArrayOf* elementsC = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dimsC.elementCount(), stringVector(), false));
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dimsC, elementsC);

    for (indexType i = 0; i < dimsC.rows(); i++) {
        for (indexType j = 0; j < dimsC.columns(); j++) {
            indexType m = i + j * b.columns();
            if (elementsA[m].isCharacterArray() && elementsB[j].isCharacterArray()) {
                std::wstring strA = elementsA[m].asWideString();
                std::wstring strB = elementsB[j].asWideString();
                elementsC[m] = ArrayOf::characterArray(strA + strB);
            } else {
                elementsC[m] = ArrayOf::empty();
            }
        }
    }
    return res;
}
//=============================================================================
ArrayOf
stringArray_plus_stringArray(const ArrayOf& a, const ArrayOf& b)
{
    // A <missing> element is not text: it goes through the element-wise path,
    // which propagates it.
    if (a.isScalar() && b.isScalar()
        && static_cast<const ArrayOf*>(a.dataPointer())->isCharacterArray()
        && static_cast<const ArrayOf*>(b.dataPointer())->isCharacterArray()) {
        std::wstring strA = a.asWideString();
        std::wstring strB = b.asWideString();
        return ArrayOf::stringArray(strA + strB);
    }
    Dimensions dimsA = a.dimensions();
    Dimensions dimsB = b.dimensions();
    Dimensions dimsC;
    if (a.isEmpty() || b.isEmpty()) {
        return broadcast_string_addition(a, b);
    }
    if (SameSizeCheck(dimsA, dimsB)) {
        return matrix_matrix_string_addition(a, b);
    }
    if (a.isScalar() || b.isScalar()) {
        if (a.isScalar()) {
            return scalar_matrix_string_addition(a, b);
        }
        return matrix_scalar_string_addition(a, b);
    }
    bool aIs2DVector = (a.rows() == 1 || a.columns() == 1);
    bool bIs2DVector = (b.rows() == 1 || b.columns() == 1);
    bool aIs2DColumn = a.columns() == 1 && a.rows() != 1;
    bool bIs2DColumn = b.columns() == 1 && b.rows() != 1;
    if (aIs2DVector && bIs2DVector && aIs2DColumn != bIs2DColumn) {
        return vector_string_addition(a, b);
    }
    return broadcast_string_addition(a, b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
