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
#include "Dimensions.hpp"
#include "SparseDynamicFunctions.hpp"
#include "Error.hpp"
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Get functions
//=============================================================================
/**
 * returns value as an array = A(index)
 * simple extraction (fast used 'for' loop)
 */
ArrayOf
ArrayOf::getValueAtIndex(uint64 index)
{
    uint64 length = (uint64)this->getLength();
    if (index >= length) {
        Error(_W("Index exceeds matrix dimensions."));
    }
    // call extration overloading here for not supported types
    Dimensions retdims(1, 1);
    if (isSparse()) {
        indexType indx = (indexType)(index - 1);
        indexType row = (indexType)(indx % getDimensionLength(0));
        indexType col = (indexType)(indx / getDimensionLength(0));
        return ArrayOf(dp->dataClass, retdims,
            GetSparseScalarElementDynamicFunction(dp->dataClass, getDimensionLength(0),
                getDimensionLength(1), dp->getData(), row + 1, col + 1),
            true);
    } else {
        int ndx = (int)index;
        void* qp = allocateArrayOf(dp->dataClass, 1, dp->fieldNames);
        copyElements(ndx, qp, 0, 1);
        return ArrayOf(dp->dataClass, retdims, qp, dp->sparse, dp->fieldNames);
    }
    // never here
    return ArrayOf();
}
//=============================================================================
/**
 * Take the current variable, and return a new array consisting of
 * the elements in source indexed by the index argument.  Indexing
 * is done using vector ordinals.
 */
ArrayOf
ArrayOf::getVectorSubset(ArrayOf& index)
{
    void* qp = nullptr;
    try {
        if (index.getLength() == 1) {
            if (index.isRowVectorCharacterArray()) {
                std::wstring str = index.getContentAsWideString();
                if (str != L":") {
                    Error(_W("index must either be real positive integers or logicals."));
                }
                ArrayOf newIndex = ArrayOf::integerRangeConstructor(
                    1, 1, dp->dimensions.getElementCount(), true);
                return getVectorSubset(newIndex);
            } else {
                double idx = index.getContentAsDoubleScalar();
                int64 iidx = (int64)idx;
                if (idx != (double)iidx || idx < 0) {
                    Error(_W("index must either be real positive integers or logicals."));
                }
                if (isSparse()) {
                    return getValueAtIndex((uint64)idx);
                } else {
                    return getValueAtIndex((uint64)(idx - 1));
                }
            }
        } else {
            if (isEmpty() && index.isEmpty()) {
                // Q = ones(3,0)
                // Q(eye(2,0))
                // Q(eye(0,2))
                return ArrayOf(
                    dp->dataClass, index.dp->dimensions, NULL, isSparse(), dp->fieldNames);
            }
            if (index.isEmpty()) {
                // Q = 1:10
                // Q(eye(2,0))
                // Q(eye(0,2))
                return ArrayOf::emptyConstructor(1, 0, isSparse());
            }
            index.toOrdinalType();
            Dimensions retdims(index.dp->dimensions);
            retdims.simplify();
            if (isSparse()) {
                if (index.getLength() == 1) {
                    indexType indx = index.getContentAsInteger32Scalar() - 1;
                    indexType row = indx % getDimensionLength(0);
                    indexType col = indx / getDimensionLength(0);
                    return ArrayOf(dp->dataClass, retdims,
                        GetSparseScalarElementDynamicFunction(dp->dataClass, getDimensionLength(0),
                            getDimensionLength(1), dp->getData(), row + 1, col + 1),
                        true);
                } else
                    return ArrayOf(dp->dataClass, retdims,
                        GetSparseVectorSubsetsDynamicFunction(dp->dataClass, getDimensionLength(0),
                            getDimensionLength(1), dp->getData(),
                            (const indexType*)index.dp->getData(), index.getDimensionLength(0),
                            index.getDimensionLength(1)),
                        true);
            }
            //
            // The output is the same size as the _index_, not the
            // source variable (neat, huh?).  But it inherits the
            // type of the source variable.
            indexType length = index.getLength();
            qp = allocateArrayOf(dp->dataClass, index.getLength(), dp->fieldNames);
            // Get a pointer to the index data set
            const indexType* index_p = (const indexType*)index.dp->getData();
            indexType bound = getLength();
            indexType ndx = 0;
            for (indexType i = 0; i < length; i++) {
                ndx = index_p[i] - 1;
                if (ndx < 0 || ndx >= bound) {
                    Error(_W("Index exceeds variable dimensions."));
                }
                copyElements(ndx, qp, i, 1);
            }
            return ArrayOf(dp->dataClass, retdims, qp, dp->sparse, dp->fieldNames);
        }
    } catch (const Exception&) {
        deleteArrayOf(qp, dp->dataClass);
        qp = nullptr;
        throw;
    }
}
//=============================================================================
/**
 * Take the current variable, and return a new array consisting of
 * the elements in source indexed by the index argument.  Indexing
 * is done using ndimensional indices.
 */
ArrayOf
ArrayOf::getNDimSubset(ArrayOfVector& index)
{
    constIndexPtr* indx = nullptr;
    void* qp = nullptr;
    indexType i;
    if (isEmpty()) {
        Error(_W("Cannot index into empty variable."));
    }
    try {
        indexType L = index.size();
        // Convert the indexing variables into an ordinal type.
        // We don't catch any exceptions - let them propogate up the
        // call chain.
        bool bEmpty = false;
        Dimensions dimsDest(L);
        for (i = 0; i < L; i++) {
            if (index[i].isEmpty()) {
                bEmpty = true;
                dimsDest[i] = 0;
            } else {
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
                indexType* idx = (indexType*)index[i].getDataPointer();
                if (idx != nullptr) {
                    dimsDest[i] = idx[index[i].getDimensions().getElementCount() - 1];
                }
            }
        }
        if (bEmpty) {
            return ArrayOf::emptyConstructor(dimsDest, isSparse());
        }
        // Set up data pointers
        indx = new_with_exception<constIndexPtr>(L);
        // Calculate the size of the output.
        Dimensions outDims(L);
        for (i = 0; i < L; i++) {
            outDims[i] = (index[i].getLength());
            indx[i] = (constIndexPtr)index[i].dp->getData();
        }
        if (outDims.getElementCount() == 0) {
            return ArrayOf::emptyConstructor(outDims, false);
        } else {
            if (isSparse()) {
                if (L > 2) {
                    Error(_W("multidimensional indexing (more than 2 dimensions) not "
                             "legal for sparse arrays"));
                }
                if ((outDims[0] == 1) && (outDims[1] == 1)) {
                    return ArrayOf(dp->dataClass, outDims,
                        GetSparseScalarElementDynamicFunction(dp->dataClass, getDimensionLength(0),
                            getDimensionLength(1), dp->getData(), *((const indexType*)indx[0]),
                            *((const indexType*)indx[1])),
                        true);
                } else {
                    return ArrayOf(dp->dataClass, outDims,
                        GetSparseNDimSubsetsDynamicFunction(dp->dataClass, getDimensionLength(0),
                            getDimensionLength(1), dp->getData(), (const indexType*)indx[0],
                            outDims[0], (const indexType*)indx[1], outDims[1]),
                        true);
                }
            }
            qp = allocateArrayOf(dp->dataClass, outDims.getElementCount(), dp->fieldNames);
            Dimensions argPointer(L);
            Dimensions currentIndex(L);
            indexType srcindex = 0;
            indexType dstindex = 0;
            while (argPointer.inside(outDims)) {
                for (indexType i = 0; i < L; i++) {
                    currentIndex[i] = (int)indx[i][argPointer[i]] - 1;
                }
                srcindex = dp->dimensions.mapPoint(currentIndex);
                copyElements(srcindex, qp, dstindex, 1);
                dstindex++;
                argPointer.incrementModulo(outDims, 0);
            }
            delete[] indx;
            indx = nullptr;
            outDims.simplify();
            return ArrayOf(dp->dataClass, outDims, qp, dp->sparse, dp->fieldNames);
        }
    } catch (const Exception&) {
        delete[] indx;
        indx = nullptr;
        deleteArrayOf(qp, dp->dataClass);
        qp = nullptr;
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
