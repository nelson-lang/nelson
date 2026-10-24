//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "ArrayOfIndexPlan.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "Decomplexify.hpp"
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::unique_ptr<bool[]>
buildIndexMap(constIndexPtr indexes, indexType indexCount, indexType dimensionLength);
//=============================================================================
static void
realWhenEmpty(ArrayOf& A);
//=============================================================================
void
ArrayOf::deleteVectorSubset(ArrayOf& arg)
{
    materializeScalar();
    if (isColonOperator(arg)) {
        // A(:) = [] empties the array, which keeps its class
        Dimensions emptyDims(0, 0);
        bool wasSparse = dp->sparse;
        std::string structTypeName = dp->classTypeName();
        dp = dp->putData(dp->dataClass, emptyDims, nullptr, false, dp->fieldNames());
        dp->setClassTypeName(structTypeName);
        realWhenEmpty(*this);
        if (wasSparse) {
            makeSparse();
        }
        return;
    }
    void* qp = nullptr;
    try {
        arg.toOrdinalType();
        if (isEmpty() && !arg.isEmpty()) {
            // nothing can be addressed in an empty array
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        if (isSparse()) {
            indexType rows = dimensionLength(0);
            indexType cols = dimensionLength(1);
            void* sparseData = sparseReadWriteDataPointer();
            void* cp = Eigen_DeleteSparseMatrixVectorSubset(dp->dataClass, rows, cols, sparseData,
                static_cast<const indexType*>(arg.dataPointer()), arg.elementCount());
            Dimensions newdim;
            newdim[0] = rows;
            newdim[1] = cols;
            dp = dp->putData(dp->dataClass, newdim, cp, true);
            realWhenEmpty(*this);
            return;
        }
        indexType N = elementCount();
        indexType i = 0;
        std::unique_ptr<bool[]> deletionMap(arg.getBinaryMap(N));
        indexType newSize = 0;
        for (i = 0; i < N; i++) {
            if (!deletionMap[i]) {
                newSize++;
            }
        }
        const stringVector& fieldNames = dp->fieldNames();
        qp = allocateArrayOf(dp->dataClass, newSize, fieldNames, true);
        indexType dstIndex = 0;
        for (i = 0; i < N; i++) {
            if (!deletionMap[i]) {
                copyElements(i, qp, dstIndex++, 1);
            }
        }
        // A vector keeps its orientation; a scalar, a matrix or an N-D array
        // becomes a row (also when nothing is deleted: A([]) = [] on a 3x3
        // matrix gives 1x9). An empty array keeps its shape.
        Dimensions newDim;
        if (dp->isVector() && !dp->isScalar()) {
            newDim = dp->dimensions_;
            if (dp->dimensions_[0] != 1) {
                newDim[0] = newSize;
            } else {
                newDim[1] = newSize;
            }
        } else if (N == 0) {
            newDim = dp->dimensions_;
        } else {
            newDim[0] = 1;
            newDim[1] = newSize;
        }
        std::string structTypeName = dp->classTypeName();
        dp = dp->putData(dp->dataClass, newDim, qp, dp->sparse, fieldNames);
        dp->setClassTypeName(structTypeName);
    } catch (const NelsonException&) {
        deleteArrayOf(qp, dp->dataClass);
        qp = nullptr;
        throw;
    }
    realWhenEmpty(*this);
}
//=============================================================================
void
ArrayOf::deleteNDimSubset(ArrayOfVector& args)
{
    materializeScalar();
    indexType singletonReferences = 0;
    indexType singletonDimension = 0;
    indexType i = 0;
    void* cp = nullptr;
    try {
        for (i = 0; i < static_cast<indexType>(args.size()); i++) {
            if (args[i].isRowVectorCharacterArray()) {
                std::wstring str = args[i].asWideString();
                if (str != L":") {
                    Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                        L"index must either be real positive integers or logicals.");
                }
            }
        }
        if (isEmpty()) {
            // an empty subscript selects nothing: the empty array is unchanged
            for (i = 0; i < static_cast<indexType>(args.size()); i++) {
                if (!args[i].isRowVectorCharacterArray() && args[i].isEmpty()) {
                    return;
                }
            }
        }
        // With fewer subscripts than dimensions the trailing dimensions are
        // folded into the last subscript; with more, the missing dimensions
        // have length 1.
        Dimensions workDims(dp->dimensions_);
        if (static_cast<indexType>(args.size()) < workDims.length()) {
            workDims = workDims.foldForSubscriptCount(args.size());
        }
        for (i = workDims.length(); i < static_cast<indexType>(args.size()); i++) {
            workDims[i] = 1;
        }
        // Exactly one subscript may be something else than the literal ':': it
        // names the dimension to delete from. A subscript covering its whole
        // dimension is not a ':' (A(1:3, 1) = [] on a 3x3 matrix is an error).
        // A(:,...,:) = [] empties the first dimension.
        singletonDimension = 0;
        for (i = 0; i < static_cast<indexType>(args.size()); i++) {
            if (!args[i].isRowVectorCharacterArray()) {
                singletonReferences++;
                singletonDimension = i;
            }
        }
        if (singletonReferences > 1) {
            Error(L"Nelson:types:invalidIndex:statementACanOnlyContainOne",
                L"Statement A(...) = [] can only contain one non-colon index.");
        }
        if (singletonReferences == 1 && args[singletonDimension].isEmpty()) {
            // nothing to delete: the array keeps its shape
            return;
        }
        if (singletonReferences == 0) {
            singletonReferences = 1;
            args[0] = ArrayOf::integerRange(1, 1, workDims[0], false);
        }
        IndexPlan plan(false, workDims, args, false);
        constIndexPtr* indexes = plan.data();
        Dimensions indexLengths = plan.outDims();
        // singletonReferences = 0 means A(:,...,:) = []: the whole variable becomes empty.
        // Otherwise the planes of the singleton dimension are deleted.
        if (singletonReferences != 0) {
            // The deletion map has one entry per plane of the singleton dimension, true when the
            // plane is deleted.
            indexType M = workDims[singletonDimension];
            std::unique_ptr<bool[]> deletionMap(
                buildIndexMap(indexes[singletonDimension], indexLengths[singletonDimension], M));
            int newSize = 0;
            for (size_t i = 0; i < static_cast<size_t>(M); i++) {
                if (!deletionMap[i]) {
                    newSize++;
                }
            }
            Dimensions retDims;
            retDims = workDims;
            retDims[singletonDimension] = newSize;
            if (isSparse()) {
                indexType rows = dimensionLength(0);
                indexType cols = dimensionLength(1);
                void* sparseData = sparseReadWriteDataPointer();
                if (singletonDimension == 0) {
                    dp = dp->putData(dp->dataClass, retDims,
                        Eigen_DeleteSparseMatrixRows(
                            dp->dataClass, rows, cols, sparseData, deletionMap.get()),
                        true);
                } else if (singletonDimension == 1) {
                    dp = dp->putData(dp->dataClass, retDims,
                        Eigen_DeleteSparseMatrixCols(
                            dp->dataClass, rows, cols, sparseData, deletionMap.get()),
                        true);
                } else {
                    Error(L"Nelson:types:sparseNDimDelete",
                        L"sparse matrices do not support deleting n-dimensional planes - Only 2-D");
                }
                realWhenEmpty(*this);
                return;
            }
            const stringVector& fieldNames = dp->fieldNames();
            cp = allocateArrayOf(dp->dataClass, retDims.elementCount(), fieldNames, true);
            // The planes kept along the deleted dimension are copied as
            // contiguous blocks: a block holds all the dimensions before it.
            indexType blockSize = 1;
            for (indexType k = 0; k < singletonDimension; k++) {
                blockSize *= workDims[k];
            }
            indexType blockCount = 1;
            for (indexType k = singletonDimension + 1; k < workDims.length(); k++) {
                blockCount *= workDims[k];
            }
            indexType srcIndex = 0;
            indexType dstIndex = 0;
            for (indexType block = 0; block < blockCount; block++) {
                for (indexType plane = 0; plane < M; plane++) {
                    if (!deletionMap[plane]) {
                        copyElements(srcIndex, cp, dstIndex, blockSize);
                        dstIndex += blockSize;
                    }
                    srcIndex += blockSize;
                }
            }
            retDims.simplify();
            std::string structTypeName = dp->classTypeName();
            dp = dp->putData(dp->dataClass, retDims, cp, dp->sparse, fieldNames);
            dp->setClassTypeName(structTypeName);
        }
    } catch (const NelsonException&) {
        deleteArrayOf(cp, dp->dataClass);
        cp = nullptr;
        throw;
    }
    realWhenEmpty(*this);
}
//=============================================================================
static std::unique_ptr<bool[]>
buildIndexMap(constIndexPtr indexes, indexType indexCount, indexType dimensionLength)
{
    std::unique_ptr<bool[]> map(new bool[dimensionLength]());
    for (indexType i = 0; i < indexCount; i++) {
        indexType index = indexes[i];
        if (index == 0 || index > dimensionLength) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        map[index - 1] = true;
    }
    return map;
}
//=============================================================================
static void
realWhenEmpty(ArrayOf& A)
{
    // the remaining elements may hold no imaginary part any more
    if (A.isComplex()) {
        A = decomplexify(A);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
