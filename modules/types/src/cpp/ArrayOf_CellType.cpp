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
//=============================================================================
namespace Nelson {
//=============================================================================
const bool
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
                    throw Exception(
                        _W("Cell definition must have same number of elements in each row"));
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
    } catch (Exception& e) {
        ArrayOf* rp = (ArrayOf*)qp;
        delete[] rp;
        rp = nullptr;
        qp = nullptr;
        e.what();
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
        throw Exception(_W("Attempt to apply contents-indexing to non-cell array object."));
    }
    if (indexing.isEmpty()) {
        throw Exception(_W("Empty contents indexing is not defined."));
    }
    if (isSparse()) {
        throw Exception(_W("getVectorContents not supported for sparse arrays."));
    }
    indexing.toOrdinalType();
    //
    // The output is the same size as the _index_, not the
    // source variable (neat, huh?).  But it inherits the
    // type of the source variable.
    //
    // The index HAS to be a scalar for contents-based addressing
    if (indexing.getLength() != 1) {
        throw Exception(_W("Content indexing must return a single value."));
    }
    constIndexPtr index_p = (constIndexPtr)indexing.dp->getData();
    if (*index_p == 0) {
        throw Exception(_W("Index exceeds cell array dimensions"));
    } else {
        indexType ndx = *index_p - 1;
        indexType bound = getLength();
        if (ndx >= bound) {
            throw Exception(_W("Index exceeds cell array dimensions"));
        }
        const ArrayOf* srcPart = (const ArrayOf*)dp->getData();
        // Make a source of whatever is in that index, and return it.
        return srcPart[ndx];
    }
}
//=============================================================================
/**
 * Return the contents of a cell array - indexed via a multi-dim index.
 */
ArrayOf
ArrayOf::getNDimContents(ArrayOfVector& indexing)
{
    if (!this->isCell()) {
        throw Exception(_W("Attempt to apply contents-indexing to non-cell array object."));
    }
    if (isSparse()) {
        throw Exception(_W("getNDimContents not supported for sparse arrays."));
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
            throw Exception(_W("Content indexing must return a single value."));
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
        throw Exception(_W("Attempt to apply contents-indexing to non cell-array object."));
    }
    if (isSparse()) {
        throw Exception(_W("getVectorContentsAsList not supported for sparse arrays."));
    }
    if (index.isEmpty()) {
        return ArrayOfVector();
    }
    if (index.isColonVectorCharacterArray()) {
        std::wstring str = index.getContentAsWideString();
        if (str != L":") {
            throw Exception(_W("index must either be real positive integers or logicals."));
        }
        index = ArrayOf::integerRangeConstructor(1, 1, dp->dimensions.getElementCount(), true);
    }
    index.toOrdinalType();
    // Get the maximum index
    indexType max_index = index.getMaxAsIndex();
    // Get our length
    indexType bound = getLength();
    if (max_index > bound) {
        throw Exception(_W("ArrayOf index exceeds bounds of cell-array"));
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
    if (!this->isCell() && !this->isStringArray()) {
        throw Exception(_W("Attempt to apply contents-indexing to non cell-array object."));
    }
    if (isSparse()) {
        throw Exception(_W("getNDimContentsAsList not supported for sparse arrays."));
    }
    // Store the return value here
    ArrayOfVector m;
    // Get the number of indexing dimensions
    indexType L = index.size();
    // Setup the data pointers
    Dimensions outDims(L);
    indexType i;
    for (i = 0; i < L; i++) {
        if (index[i].isColonVectorCharacterArray()) {
            std::wstring str = index[i].getContentAsWideString();
            if (str != L":") {
                throw Exception(_W("index must either be real positive integers or logicals."));
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
        throw Exception(_W("setVectorContents not supported for sparse arrays."));
    }
    index.toOrdinalType();
    if (index.getLength() == 0) {
        return;
    }
    if (index.getLength() != 1) {
        throw Exception(
            _W("In expression A{I} = B, I must reference a single element of cell-array A."));
    }
    constIndexPtr index_p = (constIndexPtr)index.dp->getData();
    if (*index_p == 0) {
        throw Exception(_W("Illegal negative index in expression A{I} = B."));
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
        throw Exception(_W("setNDimContents not supported for sparse arrays."));
    }
    indexType L = index.size();
    Dimensions outPos(L);
    indexType i;
    for (i = 0; i < L; i++) {
        index[i].toOrdinalType();
        if (!index[i].isScalar()) {
            throw Exception(_W("In expression A{I1,I2,...,IN} = B, (I1,...,IN) must reference a "
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
        throw Exception(_W("setVectorContentsAsList not supported for sparse arrays."));
    }
    promoteType(NLS_CELL_ARRAY);
    index.toOrdinalType();
    if ((indexType)data.size() < index.getLength()) {
        throw Exception(
            _W("Not enough right hand side values to satisy left hand side expression."));
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
        qp[ndx] = data.front();
        //      data.pop_front();
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
        throw Exception(_W("setNDimContentsAsList not supported for sparse arrays."));
    }
    promoteType(NLS_CELL_ARRAY);
    indexType L = index.size();
    // Convert the indexing variables into an ordinal type.
    indexType i;
    for (i = 0; i < L; i++) {
        index[i].toOrdinalType();
    }
    // Set up data pointers
    constIndexPtr* indx = new_with_exception<constIndexPtr>(L);
    try {
        Dimensions a(L);
        // First, we compute the maximum along each dimension.
        // We also get pointers to each of the index pointers.
        for (i = 0; i < L; i++) {
            a[i] = index[i].getMaxAsIndex();
            indx[i] = (constIndexPtr)index[i].dp->getData();
        }
        // Next, we compute the number of entries in each component.
        Dimensions argLengths(L);
        Dimensions argPointer(L);
        indexType dataCount = 1;
        for (i = 0; i < L; i++) {
            argLengths[i] = index[i].getLength();
            dataCount *= argLengths[i];
        }
        if ((int)data.size() < dataCount) {
            throw Exception(
                _W("Not enough right hand side values to satisfy left hand side expression"));
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
            for (i = 0; i < L; i++) {
                currentIndex[i] = (indexType)indx[i][argPointer[i]] - 1;
            }
            j = dp->dimensions.mapPoint(currentIndex);
            qp[j] = data.front();
            //	data.pop_front();
            data.erase(data.begin());
            argPointer.incrementModulo(argLengths, 0);
        }
        delete[] indx;
        indx = nullptr;
        dp->dimensions.simplify();
    } catch (Exception& e) {
        delete[] indx;
        indx = nullptr;
        e.what();
        throw;
    }
}
//=============================================================================
/**
 * Print this object when it is an element of a cell array.  This is
 * generally a shorthand summary of the description of the object.
 */
//=============================================================================
#define MSGBUFLEN 2048
static char msgBuffer[MSGBUFLEN];
//=============================================================================
void
ArrayOf::summarizeCellEntry(Interface *io) const
{
    if (isEmpty()) {
        if (dp->dataClass == NLS_CHAR) {
            io->outputMessage("''");
        } else {
            io->outputMessage("[]");
        }
    } else {
        switch (dp->dataClass) {
        case NLS_STRING_ARRAY:
            io->outputMessage("{");
            dp->dimensions.printMe(io);
            io->outputMessage(" string }");
            break;
        case NLS_CELL_ARRAY:
            io->outputMessage("{");
            dp->dimensions.printMe(io);
            io->outputMessage(" cell }");
            break;
        case NLS_STRUCT_ARRAY:
            io->outputMessage(" ");
            dp->dimensions.printMe(io);
            if (dp->getStructTypeName() == NLS_FUNCTION_HANDLE_STR) {
                io->outputMessage(std::string(" ") + NLS_FUNCTION_HANDLE_STR);
            } else if (dp->getStructTypeName() == NLS_STRUCT_ARRAY_STR) {
                io->outputMessage(" struct array");
            } else {
                io->outputMessage(std::string(" class ") + dp->getStructTypeName());
            }
            break;
        case NLS_CHAR: {
            Dimensions dims = dp->dimensions;
            if (dims.isRowVector()) {
                if (dims.getColumns() < (indexType)(io->getTerminalWidth() - 3)) {
                    std::wstring str = getContentAsWideString();
                    str = L"\'" + str + L"\'";
                    io->outputMessage(str);
                    return;
                }
            }
            io->outputMessage("[");
            dp->dimensions.printMe(io);
            io->outputMessage(" string]");
        } break;
        case NLS_HANDLE:
            if (dp->dimensions.isScalar()) {
                io->outputMessage("[handle]");
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" handle]");
            }
            break;
        case NLS_LOGICAL:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const logical*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" logical]");
            }
            break;
        case NLS_UINT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint8]");
            }
            break;
        case NLS_INT8:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int8*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int8]");
            }
            break;
        case NLS_UINT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint16]");
            }
            break;
        case NLS_INT16:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int16*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int16]");
            }
            break;
        case NLS_UINT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const uint32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint32]");
            }
            break;
        case NLS_INT32:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%d]", *((const int32*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int32]");
            }
            break;
        case NLS_UINT64: {
            if (dp->dimensions.isScalar()) {
                uint64 val = *((const uint64*)dp->getData());
                std::string msg = "[" + std::to_string(val) + "]";
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRIu64 "]", *((const
                // uint64*)dp->getData())); io->outputMessage(msgBuffer);
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" uint64]");
            }
        } break;
        case NLS_INT64: {
            if (dp->dimensions.isScalar()) {
                int64 value = *((const int64*)dp->getData());
                std::string msg = std::string("[") + std::to_string(value) + std::string("]");
                // snprintf(msgBuffer, MSGBUFLEN, "[" PRId64 "]", *((const
                // int64*)dp->getData()));
                io->outputMessage(msg.c_str());
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" int64]");
            }
        } break;
        case NLS_DOUBLE:
            if (!isSparse() && dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%lf]", *((const double*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" double]");
            }
            break;
        case NLS_DCOMPLEX:
            if (!isSparse() && dp->dimensions.isScalar()) {
                const double* ap = (const double*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%lf+%lfi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                if (isSparse()) {
                    io->outputMessage(" sparse");
                }
                io->outputMessage(" dcomplex]");
            }
            break;
        case NLS_SINGLE:
            if (dp->dimensions.isScalar()) {
                snprintf(msgBuffer, MSGBUFLEN, "[%f]", *((const single*)dp->getData()));
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" single]");
            }
            break;
        case NLS_SCOMPLEX:
            if (dp->dimensions.isScalar()) {
                const single* ap = (const single*)dp->getData();
                snprintf(msgBuffer, MSGBUFLEN, "[%f+%fi]", ap[0], ap[1]);
                io->outputMessage(msgBuffer);
            } else {
                io->outputMessage("[");
                dp->dimensions.printMe(io);
                io->outputMessage(" complex]");
            }
            break;
        }
    }
}
//=============================================================================
}
//=============================================================================
