//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <memory>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "Exception.hpp"
#include "NewWithException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isCell() const
{
    return (this->getDataClass() == NLS_CELL_ARRAY);
}
//=============================================================================
static ArrayOf
toCellArrayOfCharacterVectors(const stringVector& vectorStr, bool bAsColumn)
{
    const size_t nbElements = vectorStr.size();
    if (nbElements == 0) {
        Dimensions dims = bAsColumn ? Dimensions(0, 1) : Dimensions(1, 0);
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }

    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements = std::make_unique<ArrayOf[]>(nbElements);
    } catch (const std::bad_alloc&) {
        raiseError(L"Nelson:types:ERROR_MEMORY_ALLOCATION", ERROR_MEMORY_ALLOCATION);
    }
    for (size_t k = 0; k < nbElements; ++k) {
        elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
    }

    Dimensions dims = bAsColumn ? Dimensions(nbElements, 1) : Dimensions(1, nbElements);
    // Create the cell array with the allocated elements
    try {
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release(); // Only release ownership after successful construction
        return result;
    } catch (...) {
        // If construction fails, elements will be deleted by unique_ptr
        throw;
    }
}
//=============================================================================
static ArrayOf
toCellArrayOfCharacterVectors(const wstringVector& vectorStr, bool bAsColumn)
{
    const size_t nbElements = vectorStr.size();
    if (nbElements == 0) {
        Dimensions dims = bAsColumn ? Dimensions(0, 1) : Dimensions(1, 0);
        return ArrayOf(NLS_CELL_ARRAY, dims, nullptr);
    }

    std::unique_ptr<ArrayOf[]> elements;
    try {
        elements = std::make_unique<ArrayOf[]>(nbElements);
    } catch (const std::bad_alloc&) {
        raiseError(L"Nelson:types:ERROR_MEMORY_ALLOCATION", ERROR_MEMORY_ALLOCATION);
    }
    for (size_t k = 0; k < nbElements; ++k) {
        elements[k] = ArrayOf::characterArrayConstructor(vectorStr[k]);
    }

    Dimensions dims = bAsColumn ? Dimensions(nbElements, 1) : Dimensions(1, nbElements);
    try {
        ArrayOf result(NLS_CELL_ARRAY, dims, elements.get());
        elements.release(); // Only release ownership after successful construction
        return result;
    } catch (...) {
        // If construction fails, elements will be deleted by unique_ptr
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::toCellArrayOfCharacterRowVectors(const stringVector& elements)
{
    return toCellArrayOfCharacterVectors(elements, false);
}
//=============================================================================
ArrayOf
ArrayOf::toCellArrayOfCharacterRowVectors(const wstringVector& elements)
{
    return toCellArrayOfCharacterVectors(elements, false);
}
//=============================================================================
ArrayOf
ArrayOf::toCellArrayOfCharacterColumnVectors(const stringVector& elements)
{
    return toCellArrayOfCharacterVectors(elements, true);
}
//=============================================================================
ArrayOf
ArrayOf::toCellArrayOfCharacterColumnVectors(const wstringVector& elements)
{
    return toCellArrayOfCharacterVectors(elements, true);
}
//=============================================================================
bool
ArrayOf::isCellArrayOfCharacterVectors() const
{
    if (!isCell()) {
        return false;
    }
    if (isEmpty()) {
        return true;
    }
    ArrayOf* arg = (ArrayOf*)(getDataPointer());
    indexType elementCount = getElementCount();
    for (indexType k = 0; k < elementCount; k++) {
        if (!arg[k].isCharacterArray()) {
            return false;
        }
    }
    return true;
}
//=============================================================================
ArrayOf
ArrayOf::toCell(ArrayOf m)
{
    if (m.isCell()) {
        return m;
    }
    ArrayOf* elements
        = static_cast<ArrayOf*>(allocateArrayOf(NLS_CELL_ARRAY, 1, stringVector(), false));
    elements[0] = m;
    return ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 1), elements);
}
//=============================================================================
ArrayOf
ArrayOf::cellConstructor(ArrayOfMatrix& m)
{
    indexType columnCount = 0, rowCount = 0;
    ArrayOf* qp = nullptr;
    try {
        auto i = m.begin();
        while (i != m.end()) {
            ArrayOfVector ptr = *i;
            /**
             * If this is the first row in the matrix def, then we
             * record its size in columnCount.
             */
            if (i == m.begin()) {
                columnCount = ptr.size();
            } else {
                /**
                 * Otherwise, make sure the column counts are all the same...
                 */
                if (ptr.size() != columnCount) {
                    raiseError(L"Nelson:types:ERROR_CELL_DEF_MUST_HAVE_SAME_ELEMENTS_PER_ROW",
                        ERROR_CELL_DEF_MUST_HAVE_SAME_ELEMENTS_PER_ROW);
                }
            }
            ++i;
        }
        /**
         * At this point, we know how many columns our cell array has,
         * and the number of rows is also known (size of m).  So, set
         * up our dimensions, and allocate the output.
         */
        rowCount = m.size();
        Dimensions retDims(2);
        retDims[0] = rowCount;
        retDims[1] = columnCount;
        /**
         * Allocate storage space for the contents.
         */
        qp = static_cast<ArrayOf*>(
            allocateArrayOf(NLS_CELL_ARRAY, retDims.getElementCount(), stringVector(), false));
        ArrayOf* sp;
        /**
         * Loop through the rows.
         */
        sp = qp;
        i = m.begin();
        while (i != m.end()) {
            ArrayOfVector ptr = *i;
            ArrayOf* cp = sp;
            for (const auto& j : ptr) {
                *cp = j;
                cp += rowCount;
            }
            ++i;
            sp++;
        }
        return ArrayOf(NLS_CELL_ARRAY, retDims, qp);
    } catch (const Exception&) {
        auto* rp = qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        throw;
    }
}
//=============================================================================
/**
 * Return the contents of a cell array - must be a scalar...
 */
ArrayOf
ArrayOf::getVectorContents(ArrayOf& indexing)
{
    if (this->isCell()) {
        raiseError(L"Nelson:types:ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT",
            ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT);
    }
    if (indexing.isEmpty()) {
        raiseError(L"Nelson:types:ERROR_EMPTY_CONTENTS_INDEXING_NOT_DEFINED",
            ERROR_EMPTY_CONTENTS_INDEXING_NOT_DEFINED);
    }
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_GETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_GETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    indexing.toOrdinalType();
    //
    // The output is the same size as the _index_, not the
    // source variable (neat, huh?).  But it inherits the
    // type of the source variable.
    //
    // The index HAS to be a scalar for contents-based addressing
    if (indexing.getElementCount() != 1) {
        raiseError(L"Nelson:types:ERROR_CONTENT_INDEXING_MUST_RETURN_SINGLE_VALUE",
            ERROR_CONTENT_INDEXING_MUST_RETURN_SINGLE_VALUE);
    }
    auto index_p = static_cast<constIndexPtr>(indexing.dp->getData());
    if (*index_p == 0) {
        raiseError(L"Nelson:types:ERROR_INDEX_EXCEEDS_CELL_ARRAY_DIMENSIONS",
            ERROR_INDEX_EXCEEDS_CELL_ARRAY_DIMENSIONS);
    } else {
        indexType ndx = *index_p - 1;
        indexType bound = getElementCount();
        if (ndx >= bound) {
            raiseError(L"Nelson:types:ERROR_INDEX_EXCEEDS_CELL_ARRAY_DIMENSIONS",
                ERROR_INDEX_EXCEEDS_CELL_ARRAY_DIMENSIONS);
        }
        const auto* srcPart = static_cast<const ArrayOf*>(dp->getData());
        // Make a source of whatever is in that index, and return it.
        return srcPart[ndx];
    }
    return {}; // never here
}
//=============================================================================
/**
 * Return the contents of a cell array - indexed via a multi-dim index.
 */
ArrayOf
ArrayOf::getNDimContents(ArrayOfVector& indexing)
{
    if (!this->isCell()) {
        raiseError(
            L"Nelson:types:ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT_VARIANT",
            ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT_VARIANT);
    }
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_GETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_GETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    indexType L = indexing.size();
    Dimensions outPos(L);
    // Convert each of the indexing variables into an ordinal type.
    // We don't catch any exceptions - let them propogate up the
    // call chain.
    // The index HAS to be a scalar for contents-based addressing.
    for (indexType i = 0; i < L; i++) {
        indexing[i].toOrdinalType();
        if (indexing[i].getElementCount() != 1) {
            raiseError(L"Nelson:types:ERROR_CONTENT_INDEXING_MUST_RETURN_SINGLE_VALUE",
                ERROR_CONTENT_INDEXING_MUST_RETURN_SINGLE_VALUE);
        }
        auto sp = static_cast<constIndexPtr>(indexing[i].dp->getData());
        outPos[i] = *sp - 1;
    }
    indexType j = dp->dimensions.mapPoint(outPos);
    const auto* qp = static_cast<const ArrayOf*>(dp->getData());
    return qp[j];
}
//=============================================================================
/**
 * Return a subset of a cell array as a list.
 */
ArrayOfVector
ArrayOf::getVectorContentsAsList(ArrayOf& index)
{
    ArrayOfVector m;
    if (!this->isCell() && !this->isStringArray()) {
        raiseError(
            L"Nelson:types:ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT_VARIANT",
            ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT_VARIANT);
    }
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_GETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_GETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    if (index.isEmpty()) {
        return {};
    }
    if (isEmpty()) {
        return {};
    }
    if (index.isRowVectorCharacterArray()) {
        std::wstring str = index.getContentAsWideString();
        if (str != L":") {
            raiseError(
                L"Nelson:types:ERROR_INDEX_MUST_EITHER_BE_REAL_POSITIVE_INTEGERS_OR_LOGICALS",
                ERROR_INDEX_MUST_EITHER_BE_REAL_POSITIVE_INTEGERS_OR_LOGICALS);
        }
        index = ArrayOf::integerRangeConstructor(1, 1, dp->getElementCount(), true);
    }
    index.toOrdinalType();
    // Get the maximum index
    indexType max_index = index.getMaxAsIndex();
    // Get our length
    indexType bound = getElementCount();
    if (max_index > bound) {
        raiseError(L"Nelson:types:ERROR_ARRAYOF_INDEX_EXCEEDS_BOUNDS_OF_CELL_ARRAY",
            ERROR_ARRAYOF_INDEX_EXCEEDS_BOUNDS_OF_CELL_ARRAY);
    }
    // Get the length of the index object
    indexType index_length = index.getElementCount();
    // Get a pointer to the index data set
    auto index_p = static_cast<constIndexPtr>(index.dp->getData());
    // Get a pointer to our data
    const auto* qp = static_cast<const ArrayOf*>(dp->getData());
    // Now we copy data from dp to m
    for (indexType i = 0; i < index_length; i++) {
        m << qp[index_p[i] - 1];
    }
    return m;
}
//=============================================================================
/**
 * Return the contents of an cell array as a list.
 */
ArrayOfVector
ArrayOf::getNDimContentsAsList(ArrayOfVector& index)
{
    bool isStringArray = this->isStringArray();
    if (!this->isCell() && !isStringArray) {
        raiseError(L"Nelson:types:ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_OR_STRING_"
                   L"ARRAY_OBJECT",
            ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_OR_STRING_ARRAY_OBJECT);
    }
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_GETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_GETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    // Store the return value here
    ArrayOfVector m;
    // Get the number of indexing dimensions
    indexType L = index.size();
    // Setup the data pointers
    Dimensions outDims(L);
    indexType i;
    for (i = 0; i < L; i++) {
        if (index[i].isRowVectorCharacterArray()) {
            std::wstring str = index[i].getContentAsWideString();
            if (str != L":") {
                raiseError(
                    L"Nelson:types:ERROR_INDEX_MUST_EITHER_BE_REAL_POSITIVE_INTEGERS_OR_LOGICALS",
                    ERROR_INDEX_MUST_EITHER_BE_REAL_POSITIVE_INTEGERS_OR_LOGICALS);
            }
            indexType maxVal = dp->dimensions.getDimensionLength(i);
            index[i] = ArrayOf::integerRangeConstructor(1, 1, maxVal, false);
        } else {
            index[i].toOrdinalType();
        }
    }
    auto* indx = new_with_exception<constIndexPtr>(L, false);
    for (i = 0; i < L; i++) {
        outDims[i] = (index[i].getElementCount());
        indx[i] = static_cast<constIndexPtr>(index[i].dp->getData());
    }
    Dimensions argPointer(L);
    Dimensions currentIndex(L);
    const auto* qp = static_cast<const ArrayOf*>(dp->getData());
    indexType srcindex = 0;
    while (argPointer.inside(outDims)) {
        for (indexType i = 0; i < L; i++) {
            currentIndex[i] = indx[i][argPointer[i]] - 1;
        }
        srcindex = dp->dimensions.mapPoint(currentIndex);
        if (isStringArray) {
            if (!qp[srcindex].isCharacterArray()) {
                raiseError(
                    L"Nelson:types:ERROR_CONVERSION_FROM_MISSING_TO_CHARACTER_VECTOR_NOT_SUPPORTED",
                    ERROR_CONVERSION_FROM_MISSING_TO_CHARACTER_VECTOR_NOT_SUPPORTED);
            }
        }
        m << qp[srcindex];
        argPointer.incrementModulo(outDims, 0);
    }
    delete[] indx;
    indx = nullptr;
    return m;
}
//=============================================================================
/**
 * This is the vector version of the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).  Two points that make
 * this function different than replaceData are
 *   1. If the index is larger than the size, we resize to a vector of sufficient
 *      length.
 *   2. Deletions do not occur.
 */
void
ArrayOf::setVectorContents(ArrayOf& index, ArrayOf& data)
{
    promoteType(NLS_CELL_ARRAY, data.dp->fieldNames);
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_SETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_SETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    index.toOrdinalType();
    if (index.getElementCount() == 0) {
        return;
    }
    if (index.getElementCount() != 1) {
        raiseError(
            L"Nelson:types:ERROR_IN_EXPRESSION_A_BRACE_I_EQUALS_B_I_MUST_REFERENCE_SINGLE_ELEMENT",
            ERROR_IN_EXPRESSION_A_BRACE_I_EQUALS_B_I_MUST_REFERENCE_SINGLE_ELEMENT);
    }
    auto index_p = static_cast<constIndexPtr>(index.dp->getData());
    if (*index_p == 0) {
        raiseError(L"Nelson:types:ERROR_ILLEGAL_NEGATIVE_INDEX_IN_EXPRESSION_A_BRACE_I_EQUALS_B",
            ERROR_ILLEGAL_NEGATIVE_INDEX_IN_EXPRESSION_A_BRACE_I_EQUALS_B);
    }
    indexType ndx = *index_p - 1;
    vectorResize(ndx + 1);
    auto* qp = static_cast<ArrayOf*>(getReadWriteDataPointer());
    qp[ndx] = data;
    dp->dimensions.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * This is the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).
 */
void
ArrayOf::setNDimContents(ArrayOfVector& index, ArrayOf& data)
{
    promoteType(NLS_CELL_ARRAY, data.dp->fieldNames);
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_SETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_SETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    indexType L = index.size();
    Dimensions outPos(L);
    indexType i;
    for (i = 0; i < L; i++) {
        index[i].toOrdinalType();
        if (!index[i].isScalar()) {
            raiseError(L"Nelson:types:ERROR_IN_EXPRESSION_A_BRACE_I1_I2_IN_EQUALS_B_MUST_REFERENCE",
                ERROR_IN_EXPRESSION_A_BRACE_I1_I2_IN_EQUALS_B_MUST_REFERENCE);
        }
        auto sp = static_cast<constIndexPtr>(index[i].dp->getData());
        outPos[i] = *sp;
    }
    resize(outPos);
    for (i = 0; i < L; i++) {
        outPos[i] = outPos[i] - 1;
    }
    indexType j = dp->dimensions.mapPoint(outPos);
    auto* qp = static_cast<ArrayOf*>(getReadWriteDataPointer());
    qp[j] = data;
    dp->dimensions.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * This is the vector version of the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).  Two points that make
 * this function different than replaceData are
 *   1. If the index is larger than the size, we resize to a vector of sufficient
 *      length.
 *   2. Deletions do not occur.
 */
void
ArrayOf::setVectorContentsAsList(ArrayOf& index, ArrayOfVector& data)
{
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_SETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_SETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    bool asStringArray = (getDataClass() == NLS_STRING_ARRAY);
    if (asStringArray) {
        promoteType(NLS_STRING_ARRAY);
    } else {
        promoteType(NLS_CELL_ARRAY);
    }
    index.toOrdinalType();
    if (static_cast<indexType>(data.size()) < index.getElementCount()) {
        raiseError(L"Nelson:types:ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_"
                   L"SIDE_EXPRESSION",
            ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_SIDE_EXPRESSION);
    }
    // Get the maximum index
    indexType max_index = index.getMaxAsIndex();
    // Resize us as necessary.
    vectorResize(max_index);
    // Get a pointer to the data set
    auto* qp = static_cast<ArrayOf*>(getReadWriteDataPointer());
    // Get a pointer to the index data set
    auto index_p = static_cast<constIndexPtr>(index.dp->getData());
    // Get the length of the index object
    indexType index_length = index.getElementCount();
    // Copy in the data
    ArrayOf front = data.front();
    bool isCharRowVector = front.isCharacterArray() && front.isRowVector();
    bool isDoubleEmpty = front.isDoubleType(true) && front.isEmpty(true);
    for (indexType i = 0; i < index_length; i++) {
        indexType ndx = index_p[i] - 1;

        if (asStringArray) {
            if (isCharRowVector) {
                qp[ndx] = front;
            } else {
                if (isDoubleEmpty) {
                    qp[ndx] = front;
                } else {
                    raiseError(L"Nelson:types:ERROR_ASSIGNMENT_EXPECTS_A_CHARACTER_VECTOR",
                        ERROR_ASSIGNMENT_EXPECTS_A_CHARACTER_VECTOR);
                }
            }
        } else {
            qp[ndx] = front;
        }
        data.pop_front();
        if (data.size() > 0) {
            front = data.front();
        }
    }
    dp->dimensions.simplify();
    dp->refreshDimensionCache();
}
//=============================================================================
/**
 * This is the multidimensional cell-replacement function.
 * This is for content-based indexing (curly brackets).
 */
void
ArrayOf::setNDimContentsAsList(ArrayOfVector& index, ArrayOfVector& data)
{
    if (isSparse()) {
        raiseError(L"Nelson:types:ERROR_SETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS",
            ERROR_SETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS);
    }
    bool asStringArray = (getDataClass() == NLS_STRING_ARRAY);
    if (asStringArray) {
        promoteType(NLS_STRING_ARRAY);
    } else {
        promoteType(NLS_CELL_ARRAY);
    }
    indexType L = index.size();
    // Convert the indexing variables into an ordinal type.
    for (indexType i = 0; i < L; i++) {
        index[i].toOrdinalType();
    }
    // Set up data pointers
    auto* indx = new_with_exception<constIndexPtr>(L, false);
    try {
        Dimensions a(L);
        // First, we compute the maximum along each dimension.
        // We also get pointers to each of the index pointers.
        for (indexType i = 0; i < L; i++) {
            a[i] = index[i].getMaxAsIndex();
            indx[i] = static_cast<constIndexPtr>(index[i].dp->getData());
        }
        // Next, we compute the number of entries in each component.
        Dimensions argLengths(L);
        Dimensions argPointer(L);
        indexType dataCount = 1;
        for (indexType i = 0; i < L; i++) {
            argLengths[i] = index[i].getElementCount();
            dataCount *= argLengths[i];
        }
        if (static_cast<int>(data.size()) < dataCount) {
            raiseError(L"Nelson:types:ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_"
                       L"SIDE_EXPRESSION_VARIANT",
                ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_SIDE_EXPRESSION_VARIANT);
        }
        // Resize us as necessary
        resize(a);
        // Get a writable data pointer
        auto* qp = static_cast<ArrayOf*>(getReadWriteDataPointer());
        // Now, we copy data from dp to our real part,
        // computing indices along the way.
        Dimensions currentIndex(dp->dimensions.getLength());
        indexType j;
        while (argPointer.inside(argLengths)) {
            ArrayOf front = data.front();
            for (indexType i = 0; i < L; i++) {
                currentIndex[i] = indx[i][argPointer[i]] - 1;
            }
            j = dp->dimensions.mapPoint(currentIndex);
            if (asStringArray) {
                if (front.isCharacterArray() && (front.isRowVector() || front.isEmpty())) {
                    qp[j] = front;
                } else {
                    if (front.isDoubleType(true) && front.isEmpty(true)) {
                        qp[j] = front;
                    } else {
                        raiseError(L"Nelson:types:ERROR_ASSIGNMENT_EXPECTS_A_CHARACTER_VECTOR",
                            ERROR_ASSIGNMENT_EXPECTS_A_CHARACTER_VECTOR);
                    }
                }
            } else {
                qp[j] = front;
            }
            data.pop_front();
            argPointer.incrementModulo(argLengths, 0);
        }
        delete[] indx;
        indx = nullptr;
        dp->dimensions.simplify();
        dp->refreshDimensionCache();
    } catch (const Exception&) {
        delete[] indx;
        indx = nullptr;
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
