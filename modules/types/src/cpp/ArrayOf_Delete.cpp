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
// Delete functions
//=============================================================================
/**
 * Delete a vector subset of a variable.
 */
void
ArrayOf::deleteVectorSubset(ArrayOf& arg)
{
    void* qp = nullptr;
    bool* deletionMap = nullptr;
    try {
        // First convert arg to an ordinal type.
        arg.toOrdinalType();
        if (isSparse()) {
            indexType rows = getDimensionLength(0);
            indexType cols = getDimensionLength(1);
            void* cp = DeleteSparseMatrixVectorSubsetDynamicFunction(dp->dataClass, rows, cols,
                dp->getData(), (const indexType*)arg.getDataPointer(), arg.getLength());
            Dimensions newdim;
            newdim[0] = rows;
            newdim[1] = cols;
            dp = dp->putData(dp->dataClass, newdim, cp, true);
            return;
        }
        // Next, build a deletion map.
        indexType N = getLength();
        indexType i = 0;
        deletionMap = arg.getBinaryMap(N);
        // Now, we count up the number of elements that remain after deletion.
        indexType newSize = 0;
        for (i = 0; i < N; i++) {
            if (!deletionMap[i]) {
                newSize++;
            }
        }
        // Allocate a new space to hold the data.
        qp = allocateArrayOf(dp->dataClass, newSize, dp->fieldNames);
        // Loop through the indices - copy elements in that
        // have not been deleted.
        indexType dstIndex = 0;
        for (i = 0; i < N; i++) {
            if (!deletionMap[i]) {
                copyElements(i, qp, dstIndex++, 1);
            }
        }
        delete[] deletionMap;
        deletionMap = nullptr;
        Dimensions newDim;
        if (dp->dimensions.isScalar()) {
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = newSize;
        } else if (dp->dimensions.isVector()) {
            newDim = dp->dimensions;
            if (dp->dimensions[0] != 1) {
                newDim[0] = newSize;
            } else {
                newDim[1] = newSize;
            }
        } else {
            newDim.reset();
            if (newSize == 0) {
                newDim[0] = 0;
                newDim[1] = 0;
            } else {
                newDim[0] = 1;
                newDim[1] = newSize;
            }
        }
        dp = dp->putData(dp->dataClass, newDim, qp, dp->sparse, dp->fieldNames);
    } catch (const Exception&) {
        deleteArrayOf(qp, dp->dataClass);
        qp = nullptr;
        delete[] deletionMap;
        deletionMap = nullptr;
        throw;
    }
}
//=============================================================================
/**
 * Delete a subset of a variable.
 */
void
ArrayOf::deleteNDimSubset(ArrayOfVector& args)
{
    indexType singletonReferences = 0;
    indexType singletonDimension = 0;
    indexType i = 0;
    ArrayOf qp;
    bool* indxCovered = nullptr;
    bool* deletionMap = nullptr;
    void* cp = nullptr;
    try {
        // Our strategy is as follows.  To make the deletion, we need
        // one piece of information: the dimension to delete.
        // To do so, we first make a pass through the set of arguments,
        // checking each one to see if it "covers" its index set.
        //
        // However, to simplify the testing of
        // conditions later on, we must make sure that the length of
        // the index list matches our number of dimensions.  We extend
        // it using 1 references, and throw an exception if there are
        // more indices than our dimension set.
        for (i = 0; i < (indexType)args.size(); i++) {
            if (args[i].isRowVectorCharacterArray()) {
                std::wstring str = args[i].getContentAsWideString();
                if (str != L":") {
                    Error(_W("index must either be real positive integers or logicals."));
                }
                indexType maxVal = dp->dimensions.getDimensionLength(i);
                args[i] = ArrayOf::integerRangeConstructor(1, 1, maxVal, false);
            }
            args[i].toOrdinalType();
        }
        // First, add enough "1" singleton references to pad the
        // index set out to the size of our variable.
        if ((indexType)args.size() < dp->dimensions.getLength())
            for (i = args.size(); i < dp->dimensions.getLength(); i++) {
                args.push_back(ArrayOf::uint32Constructor(1));
            }
        // Now cycle through indices one at a time.  Count
        // the number of non-covering indices.  Also track the
        // location of the last-occurring non-covering index.
        for (i = 0; i < (indexType)args.size(); i++) {
            qp = args[i];
            // Get a binary representation of each index over the range
            // [0,dimensions[i]-1]
            indxCovered = qp.getBinaryMap(dp->dimensions[i]);
            // Scan the array, and make sure all elements are true.  If not,
            // then this is the "singleton" dimension.  Kick the singleton
            // reference counter, and record the current dimension.
            bool allCovered = true;
            for (indexType k = 0; allCovered && (k < dp->dimensions[i]); k++) {
                allCovered = allCovered && indxCovered[k];
            }
            delete[] indxCovered;
            indxCovered = nullptr;
            if (!allCovered) {
                singletonReferences++;
                singletonDimension = i;
            }
        }
        // Now, we check the number of singleton references we
        // encountered.  There are three cases to check:
        //  Case 1. No singleton references.  This is OK - it
        //	      means we wish to delete the entire variable.
        //	      We set a flag, and proceed to validate the covering
        //	      of each dimension by its corresponding index set.
        //  Case 2. One singleton reference.  This is OK - it
        //	      means we wish to delete a single plane of
        //	      data.  Retrieve the index of the plane, and store
        //	      it in singletonIndex.
        //  Case 3. Two or more singleton references.  Can't do it -
        //	      throw an error.
        if (singletonReferences > 1) {
            Error(_W("Statement A(...) = [] can only contain one non-colon index."));
        }
        if (singletonReferences == 0) {
            singletonDimension = -1;
        }
        // If we got this far, the user either entered an expression like
        // A(:,:,...,:,s,:,...,:) = [], or something numerically equivalent,
        // or the user entered something like A(:,...,:) = [].
        // In the latter case (indicated by singletonReferences = 0), we simply
        // delete the entire variable, and make it an empty type.
        // In the former case, we will have more work to do...
        if (singletonReferences != 0) {
            // We have to rescan our (now-identified) singleton
            // dimension to build a deletion map.  The map is
            // marked true for each plane we wish to delete.
            // The map is the size of the _data_'s dimension.
            indexType M = dp->dimensions[singletonDimension];
            deletionMap = args[singletonDimension].getBinaryMap(M);
            // We can now calculate the new size of the variable in the
            // singletonDimension by counting the number of "false" entries in
            // deletionMap.
            int newSize = 0;
            for (size_t i = 0; i < (size_t)M; i++) {
                if (!deletionMap[i]) {
                    newSize++;
                }
            }
            indexType rowCount = dp->dimensions[0];
            Dimensions retDims;
            // Copy our current dimensions to the output dimensions.
            retDims = dp->dimensions;
            // Update the singleton dimension to the new size.
            retDims[singletonDimension] = newSize;
            // For sparse matrices, we branch here to call the sparse matrix deletion
            // code
            if (isSparse()) {
                indexType rows = getDimensionLength(0);
                indexType cols = getDimensionLength(1);
                if (singletonDimension == 0) {
                    dp = dp->putData(dp->dataClass, retDims,
                        DeleteSparseMatrixRowsDynamicFunction(
                            dp->dataClass, rows, cols, dp->getData(), deletionMap),
                        true);
                } else if (singletonDimension == 1) {
                    dp = dp->putData(dp->dataClass, retDims,
                        DeleteSparseMatrixColsDynamicFunction(
                            dp->dataClass, rows, cols, dp->getData(), deletionMap),
                        true);
                } else {
                    Error(_W("sparse matrices do not support deleting "
                             "n-dimensional planes - Only 2-D"));
                }
                delete[] deletionMap;
                deletionMap = nullptr;
                delete[] indxCovered;
                indxCovered = nullptr;
                return;
            }
            // Allocate space for the return objects data
            cp = allocateArrayOf(dp->dataClass, retDims.getElementCount(), dp->fieldNames);
            // Track our offset into the original data & our offset into
            // the truncated data.
            indexType srcIndex = 0;
            indexType dstIndex = 0;
            // Inintialize an ND pointer to the first element in the
            // current data structure.
            indexType L = dp->dimensions.getLength();
            Dimensions curPos(L);
            // Loop until we have exhausted the original data.
            while (curPos.inside(dp->dimensions)) {
                // Check to see if this column is to be skipped
                if (!deletionMap[curPos[singletonDimension]]) {
                    // Copy the data from our original data structure to the
                    // new data structure, starting from srcIndex, and
                    // copying to dstIndex.
                    copyElements(srcIndex, cp, dstIndex, 1);
                    // Advance the destination pointer. - we only do this on a copy
                    dstIndex++;
                }
                // Advance the source pointer - we always do this
                srcIndex++;
                curPos.incrementModulo(dp->dimensions, 0);
            }
            delete[] deletionMap;
            deletionMap = nullptr;
            retDims.simplify();
            dp = dp->putData(dp->dataClass, retDims, cp, dp->sparse, dp->fieldNames);
        } else {
            /* here we need to return empty mxn and not only 0x0*/
            /*
            A = [0 2 1 ;
            3 4 5];
            A([1 2],:) = []
            */
            Dimensions newDims(0, 0);
            Dimensions d = getDimensions();
            indexType m = d[0];
            for (size_t k = 1; k < d.getLength(); ++k) {
                if (m < d[k]) {
                    m = d[k];
                }
            }
            indexType idxm = 0;
            for (size_t k = 0; k < d.getLength(); ++k) {
                if (m == d[k]) {
                    break;
                } else {
                    idxm++;
                }
            }
            newDims[idxm] = m;
            dp = dp->putData(dp->dataClass, newDims, NULL, dp->sparse, dp->fieldNames);
        }
    } catch (const Exception&) {
        delete[] deletionMap;
        deletionMap = nullptr;
        deleteArrayOf(cp, dp->dataClass);
        cp = nullptr;
        delete[] indxCovered;
        indxCovered = nullptr;
        throw;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
