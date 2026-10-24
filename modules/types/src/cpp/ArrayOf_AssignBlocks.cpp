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
#include "Dimensions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
firstDimensionRun(const indexType outDims[maxDims], const indexType srcDims[maxDims],
    constIndexPtr* indexes, indexType colonIndex, indexType& firstRow);
//=============================================================================
/**
 * Writes A(I1,I2,...) = B as one contiguous block per column when the first
 * subscript is ':' or an ascending unit-stride run (a:b): A(:,J), A(a:b,J),
 * A(a:b,c:d), A(a:b,:,k), ... Returns false (nothing written) otherwise, and
 * for a scalar right-hand side; the caller then runs the element-wise loop.
 * The destination has already been resized to hold every selected element.
 */
bool
ArrayOf::trySetContiguousBlocks(ArrayOf& rightData, void* destination,
    const indexType outDims[maxDims], const indexType srcDims[maxDims], constIndexPtr* indexes,
    indexType indexCount, indexType colonIndex, indexType advance)
{
    if (advance != 1 || indexCount < 2 || indexCount > maxDims) {
        return false;
    }
    indexType firstRow = 0;
    if (!firstDimensionRun(outDims, srcDims, indexes, colonIndex, firstRow)) {
        return false;
    }
    const indexType blockSize = outDims[0];
    indexType strides[maxDims];
    strides[0] = 1;
    for (indexType i = 1; i < indexCount; i++) {
        strides[i] = strides[i - 1] * srcDims[i - 1];
    }
    indexType blockCount = 1;
    for (indexType i = 1; i < indexCount; i++) {
        blockCount *= outDims[i];
    }
    if (blockCount == 0 || rightData.elementCount() != blockSize * blockCount) {
        return false;
    }
    for (indexType i = 1; i < indexCount; i++) {
        constIndexPtr dimIndexes = indexes[i];
        if (dimIndexes == nullptr) {
            // the first preserved colon: the full extent, implicitly in range
            continue;
        }
        for (indexType k = 0; k < outDims[i]; k++) {
            if (dimIndexes[k] == 0 || dimIndexes[k] > srcDims[i]) {
                return false;
            }
        }
    }
    indexType pointer[maxDims] = { 0 };
    indexType srcIndex = 0;
    for (indexType block = 0; block < blockCount; block++) {
        indexType dstIndex = firstRow;
        for (indexType i = 1; i < indexCount; i++) {
            constIndexPtr dimIndexes = indexes[i];
            const indexType selected
                = (dimIndexes == nullptr) ? pointer[i] : dimIndexes[pointer[i]] - 1;
            dstIndex += selected * strides[i];
        }
        rightData.copyElements(srcIndex, destination, dstIndex, blockSize);
        srcIndex += blockSize;
        for (indexType i = 1; i < indexCount; i++) {
            if (++pointer[i] < outDims[i]) {
                break;
            }
            pointer[i] = 0;
        }
    }
    return true;
}
//=============================================================================
static bool
firstDimensionRun(const indexType outDims[maxDims], const indexType srcDims[maxDims],
    constIndexPtr* indexes, indexType colonIndex, indexType& firstRow)
{
    const indexType blockSize = outDims[0];
    if (blockSize == 0) {
        return false;
    }
    if (colonIndex == 0) {
        firstRow = 0;
        return blockSize == srcDims[0];
    }
    constIndexPtr rows = indexes[0];
    if (rows == nullptr || rows[0] == 0) {
        return false;
    }
    for (indexType k = 1; k < blockSize; k++) {
        if (rows[k] != rows[k - 1] + 1) {
            return false;
        }
    }
    firstRow = rows[0] - 1;
    return firstRow + blockSize <= srcDims[0];
}
//=============================================================================
} // namespace Nelson
//=============================================================================
