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
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isCell() const
{
    return (this->getDataClass() == NLS_CELL_ARRAY);
}
//=============================================================================
ArrayOf
ArrayOf::toCell(ArrayOf m)
{
    if (m.isCell()) {
        return m;
    }
    ArrayOf* elements = (ArrayOf*)allocateArrayOf(NLS_CELL_ARRAY, 1);
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
        ArrayOfMatrix::iterator i = m.begin();
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
                    Error(_W("Cell definition must have same number of elements in each row"));
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
        qp = (ArrayOf*)allocateArrayOf(NLS_CELL_ARRAY, retDims.getElementCount());
        ArrayOf* sp;
        /**
         * Loop through the rows.
         */
        sp = qp;
        i = m.begin();
        while (i != m.end()) {
            ArrayOfVector ptr = *i;
            ArrayOf* cp = sp;
            for (sizeType j = 0; j < (sizeType)ptr.size(); j++) {
                *cp = ptr[j];
                cp += rowCount;
            }
            ++i;
            sp++;
        }
        return ArrayOf(NLS_CELL_ARRAY, retDims, qp);
    } catch (const Exception&) {
        ArrayOf* rp = (ArrayOf*)qp;
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
        Error(_W("Attempt to apply contents-indexing to non-cell array object."));
    }
    if (indexing.isEmpty()) {
        Error(_W("Empty contents indexing is not defined."));
    }
    if (isSparse()) {
        Error(_W("getVectorContents not supported for sparse arrays."));
    }
    indexing.toOrdinalType();
    //
    // The output is the same size as the _index_, not the
    // source variable (neat, huh?).  But it inherits the
    // type of the source variable.
    //
    // The index HAS to be a scalar for contents-based addressing
    if (indexing.getLength() != 1) {
        Error(_W("Content indexing must return a single value."));
    }
    constIndexPtr index_p = (constIndexPtr)indexing.dp->getData();
    if (*index_p == 0) {
        Error(_W("Index exceeds cell array dimensions"));
    } else {
        indexType ndx = *index_p - 1;
        indexType bound = getLength();
        if (ndx >= bound) {
            Error(_W("Index exceeds cell array dimensions"));
        }
        const ArrayOf* srcPart = (const ArrayOf*)dp->getData();
        // Make a source of whatever is in that index, and return it.
        return srcPart[ndx];
    }
    return ArrayOf(); // never here
}
//=============================================================================
/**
 * Return the contents of a cell array - indexed via a multi-dim index.
 */
ArrayOf
ArrayOf::getNDimContents(ArrayOfVector& indexing)
{
    if (!this->isCell()) {
        Error(_W("Attempt to apply contents-indexing to non-cell array object."));
    }
    if (isSparse()) {
        Error(_W("getNDimContents not supported for sparse arrays."));
    }
    indexType L = indexing.size();
    Dimensions outPos(L);
    // Convert each of the indexing variables into an ordinal type.
    // We don't catch any exceptions - let them propogate up the
    // call chain.
    // The index HAS to be a scalar for contents-based addressing.
    for (indexType i = 0; i < L; i++) {
        indexing[i].toOrdinalType();
        if (indexing[i].getLength() != 1) {
            Error(_W("Content indexing must return a single value."));
        }
        constIndexPtr sp = (constIndexPtr)indexing[i].dp->getData();
        outPos[i] = *sp - 1;
    }
    indexType j = dp->dimensions.mapPoint(outPos);
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
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
        Error(_W("Attempt to apply contents-indexing to non cell-array object."));
    }
    if (isSparse()) {
        Error(_W("getVectorContentsAsList not supported for sparse arrays."));
    }
    if (index.isEmpty()) {
        return ArrayOfVector();
    }
    if (index.isRowVectorCharacterArray()) {
        std::wstring str = index.getContentAsWideString();
        if (str != L":") {
            Error(_W("index must either be real positive integers or logicals."));
        }
        index = ArrayOf::integerRangeConstructor(1, 1, dp->dimensions.getElementCount(), true);
    }
    index.toOrdinalType();
    // Get the maximum index
    indexType max_index = index.getMaxAsIndex();
    // Get our length
    indexType bound = getLength();
    if (max_index > bound) {
        Error(_W("ArrayOf index exceeds bounds of cell-array"));
    }
    // Get the length of the index object
    indexType index_length = index.getLength();
    // Get a pointer to the index data set
    constIndexPtr index_p = (constIndexPtr)index.dp->getData();
    // Get a pointer to our data
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    // Now we copy data from dp to m
    for (indexType i = 0; i < index_length; i++) {
        m.push_back(qp[index_p[i] - 1]);
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
        Error(_W("Attempt to apply contents-indexing to non cell or string array object."));
    }
    if (isSparse()) {
        Error(_W("getNDimContentsAsList not supported for sparse arrays."));
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
                Error(_W("index must either be real positive integers or logicals."));
            }
            indexType maxVal = dp->dimensions.getDimensionLength(i);
            index[i] = ArrayOf::integerRangeConstructor(1, 1, maxVal, false);
        } else {
            index[i].toOrdinalType();
        }
    }
    constIndexPtr* indx = new_with_exception<constIndexPtr>(L);
    for (i = 0; i < L; i++) {
        outDims[i] = (index[i].getLength());
        indx[i] = (constIndexPtr)index[i].dp->getData();
    }
    Dimensions argPointer(L);
    Dimensions currentIndex(L);
    const ArrayOf* qp = (const ArrayOf*)dp->getData();
    indexType srcindex = 0;
    while (argPointer.inside(outDims)) {
        for (indexType i = 0; i < L; i++) {
            currentIndex[i] = indx[i][argPointer[i]] - 1;
        }
        srcindex = dp->dimensions.mapPoint(currentIndex);
        if (isStringArray) {
            if (!qp[srcindex].isCharacterArray()) {
                Error(_W("Conversion from <missing> to character vector is not supported."));
            }
        }
        m.push_back(qp[srcindex]);
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
        Error(_W("setVectorContents not supported for sparse arrays."));
    }
    index.toOrdinalType();
    if (index.getLength() == 0) {
        return;
    }
    if (index.getLength() != 1) {
        Error(_W("In expression A{I} = B, I must reference a single element of cell-array A."));
    }
    constIndexPtr index_p = (constIndexPtr)index.dp->getData();
    if (*index_p == 0) {
        Error(_W("Illegal negative index in expression A{I} = B."));
    }
    indexType ndx = *index_p - 1;
    vectorResize(ndx + 1);
    ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
    qp[ndx] = data;
    dp->dimensions.simplify();
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
        Error(_W("setNDimContents not supported for sparse arrays."));
    }
    indexType L = index.size();
    Dimensions outPos(L);
    indexType i;
    for (i = 0; i < L; i++) {
        index[i].toOrdinalType();
        if (!index[i].isScalar()) {
            Error(_W("In expression A{I1,I2,...,IN} = B, (I1,...,IN) must reference a "
                     "single element of cell-array A."));
        }
        constIndexPtr sp = (constIndexPtr)index[i].dp->getData();
        outPos[i] = *sp;
    }
    resize(outPos);
    for (i = 0; i < L; i++) {
        outPos[i] = outPos[i] - 1;
    }
    indexType j = dp->dimensions.mapPoint(outPos);
    ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
    qp[j] = data;
    dp->dimensions.simplify();
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
        Error(_W("setVectorContentsAsList not supported for sparse arrays."));
    }
    bool asStringArray = (getDataClass() == NLS_STRING_ARRAY);
    if (asStringArray) {
        promoteType(NLS_STRING_ARRAY);
    } else {
        promoteType(NLS_CELL_ARRAY);
    }
    index.toOrdinalType();
    if ((indexType)data.size() < index.getLength()) {
        Error(_W("Not enough right hand side values to satisy left hand side expression."));
    }
    // Get the maximum index
    indexType max_index = index.getMaxAsIndex();
    // Resize us as necessary.
    vectorResize(max_index);
    // Get a pointer to the dataset
    ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
    // Get a pointer to the index data set
    constIndexPtr index_p = (constIndexPtr)index.dp->getData();
    // Get the length of the index object
    indexType index_length = index.getLength();
    // Copy in the data
    for (indexType i = 0; i < index_length; i++) {
        indexType ndx = index_p[i] - 1;

        if (asStringArray) {
            if (data.front().isCharacterArray() && data.front().isRowVector()) {
                qp[ndx] = data.front();
            } else {
                if (data.front().isDoubleType(true) && data.front().isEmpty(true)) {
                    qp[ndx] = data.front();
                } else {
                    Error(_W("{} assignment expects a character vector."));
                }
            }
        } else {
            qp[ndx] = data.front();
        }

        data.erase(data.begin());
    }
    dp->dimensions.simplify();
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
        Error(_W("setNDimContentsAsList not supported for sparse arrays."));
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
    constIndexPtr* indx = new_with_exception<constIndexPtr>(L);
    try {
        Dimensions a(L);
        // First, we compute the maximum along each dimension.
        // We also get pointers to each of the index pointers.
        for (indexType i = 0; i < L; i++) {
            a[i] = index[i].getMaxAsIndex();
            indx[i] = (constIndexPtr)index[i].dp->getData();
        }
        // Next, we compute the number of entries in each component.
        Dimensions argLengths(L);
        Dimensions argPointer(L);
        indexType dataCount = 1;
        for (indexType i = 0; i < L; i++) {
            argLengths[i] = index[i].getLength();
            dataCount *= argLengths[i];
        }
        if ((int)data.size() < dataCount) {
            Error(_W("Not enough right hand side values to satisfy left hand side expression"));
        }
        // Resize us as necessary
        resize(a);
        // Get a writable data pointer
        ArrayOf* qp = (ArrayOf*)getReadWriteDataPointer();
        // Now, we copy data from dp to our real part,
        // computing indices along the way.
        Dimensions currentIndex(dp->dimensions.getLength());
        indexType j;
        while (argPointer.inside(argLengths)) {
            for (indexType i = 0; i < L; i++) {
                currentIndex[i] = (indexType)indx[i][argPointer[i]] - 1;
            }
            j = dp->dimensions.mapPoint(currentIndex);
            if (asStringArray) {
                if (data.front().isCharacterArray() && data.front().isRowVector()) {
                    qp[j] = data.front();
                } else {
                    if (data.front().isDoubleType(true) && data.front().isEmpty(true)) {
                        qp[j] = data.front();
                    } else {
                        Error(_W("{} assignment expects a character vector."));
                    }
                }
            } else {
                qp[j] = data.front();
            }

            data.erase(data.begin());
            argPointer.incrementModulo(argLengths, 0);
        }
        delete[] indx;
        indx = nullptr;
        dp->dimensions.simplify();
    } catch (const Exception&) {
        delete[] indx;
        indx = nullptr;
        throw;
    }
}
//=============================================================================
}
//=============================================================================
