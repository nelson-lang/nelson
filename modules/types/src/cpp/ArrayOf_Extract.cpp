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
#include "ArrayOfView.hpp"
#include "Data.hpp"
#include "Dimensions.hpp"
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
#include "Decomplexify.hpp"
#include <cmath>
#include <complex>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
preserveClassType(const ArrayOf& source, ArrayOf result);
static NelsonType
emptySubsetClass(NelsonType dataClass);
static Dimensions
emptyLinearSubsetDimensions(Dimensions source, Dimensions index);
//=============================================================================
static bool
isAscendingUnitStrideIndex(constIndexPtr index, indexType length);
//=============================================================================
static NelsonType
ordinalIndexClassForView();
//=============================================================================
static bool
isFullUnitStrideIndex(constIndexPtr index, indexType length, indexType sourceLength);
//=============================================================================
static indexType
dimensionStride(const Dimensions& dimensions, indexType dimension);
//=============================================================================
static indexType
leadingFullDimensionCount(const Dimensions& sourceDims, const std::vector<constIndexPtr>& indexes,
    const Dimensions& outDims, indexType indexCount);
//=============================================================================
static bool
allIndexesWithinBounds(const Dimensions& sourceDims, const std::vector<constIndexPtr>& indexes,
    const Dimensions& outDims, indexType firstDim, indexType indexCount);
//=============================================================================
static bool
copyContiguousBlocks(ArrayOf& source, const Dimensions& sourceDims,
    const std::vector<constIndexPtr>& indexes, indexType indexCount, const Dimensions& outDims,
    void* destination);
//=============================================================================
ArrayOf
ArrayOf::valueAtIndex(uint64 index)
{
    const auto type = dataClass();
    if ((type == NLS_DOUBLE || type == NLS_SINGLE || type == NLS_LOGICAL) && !isSparse()) {
        if (index >= static_cast<uint64>(elementCount())) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        if (type == NLS_LOGICAL) {
            const logical value = static_cast<const logical*>(dataPointer())[index];
            return ArrayOf(Representation::InlineScalar, value);
        }
        if (type == NLS_SINGLE) {
            return ArrayOf::singleScalar(static_cast<const single*>(dataPointer())[index]);
        }
        return ArrayOf::doubleScalar(static_cast<const double*>(dataPointer())[index]);
    }
    ArrayOf res;
    Dimensions retdims(1, 1);
    if (isSparse()) {
        // index is 0-based, like the dense branch below.
        auto length = static_cast<uint64>(this->elementCount());
        if (index >= length) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        auto indx = static_cast<indexType>(index);
        auto row = static_cast<indexType>(indx % dimensionLength(0));
        auto col = static_cast<indexType>(indx / dimensionLength(0));
        res = ArrayOf(dp->dataClass, retdims,
            Eigen_GetSparseScalarElement(dp->dataClass, dimensionLength(0), dimensionLength(1),
                dp->data(), row + 1, col + 1),
            true);
    } else {
        auto length = static_cast<uint64>(this->elementCount());
        if (index >= length) {
            Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
        }
        indexType ndx = static_cast<indexType>(index);
        Dimensions scalarDims(1, 1);
        switch (dp->dataClass) {
        case NLS_UINT8: {
            const auto* data = static_cast<const uint8*>(dp->data());
            res = ArrayOf(NLS_UINT8, scalarDims, &data[ndx], sizeof(uint8));
        } break;
        case NLS_INT8: {
            const auto* data = static_cast<const int8*>(dp->data());
            res = ArrayOf(NLS_INT8, scalarDims, &data[ndx], sizeof(int8));
        } break;
        case NLS_UINT16: {
            const auto* data = static_cast<const uint16*>(dp->data());
            res = ArrayOf(NLS_UINT16, scalarDims, &data[ndx], sizeof(uint16));
        } break;
        case NLS_INT16: {
            const auto* data = static_cast<const int16*>(dp->data());
            res = ArrayOf(NLS_INT16, scalarDims, &data[ndx], sizeof(int16));
        } break;
        case NLS_UINT32: {
            const auto* data = static_cast<const uint32*>(dp->data());
            res = ArrayOf(NLS_UINT32, scalarDims, &data[ndx], sizeof(uint32));
        } break;
        case NLS_INT32: {
            const auto* data = static_cast<const int32*>(dp->data());
            res = ArrayOf(NLS_INT32, scalarDims, &data[ndx], sizeof(int32));
        } break;
        case NLS_UINT64: {
            const auto* data = static_cast<const uint64*>(dp->data());
            res = ArrayOf(NLS_UINT64, scalarDims, &data[ndx], sizeof(uint64));
        } break;
        case NLS_INT64: {
            const auto* data = static_cast<const int64*>(dp->data());
            res = ArrayOf(NLS_INT64, scalarDims, &data[ndx], sizeof(int64));
        } break;
        case NLS_SCOMPLEX: {
            const auto* data = static_cast<const single*>(dp->data());
            res = ArrayOf(NLS_SCOMPLEX, scalarDims, &data[ndx * 2], 2 * sizeof(single));
        } break;
        case NLS_DCOMPLEX: {
            const auto* data = static_cast<const double*>(dp->data());
            res = ArrayOf(NLS_DCOMPLEX, scalarDims, &data[ndx * 2], 2 * sizeof(double));
        } break;
        default: {
            const stringVector& fieldNames = dp->fieldNames();
            void* qp = allocateArrayOf(dp->dataClass, 1, fieldNames, false);
            copyElements(ndx, qp, 0, 1);
            res = ArrayOf(dp->dataClass, retdims, qp, dp->sparse, fieldNames);
        } break;
        }
    }
    res = preserveClassType(*this, res);
    if (res.isComplex()) {
        return decomplexify(res);
    }
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::vectorSubset(ArrayOf& index)
{
    void* qp = nullptr;
    try {
        if (index.elementCount() == 1) {
            if (index.isRowVectorCharacterArray()) {
                std::wstring str = index.asWideString();
                if (str != L":") {
                    Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                        L"index must either be real positive integers or logicals.");
                }
                ArrayOf newIndex = ArrayOf::integerRange(1, 1, elementCount(), true);
                ArrayOf result = vectorSubset(newIndex);
                Dimensions colonDimensions(elementCount(), 1);
                result.reshape(colonDimensions);
                return result;
            }
            if (index.isLogical()) {
                logical idx = index.asLogicalScalar();
                if (idx) {
                    return valueAtIndex(0);
                }
                Dimensions dims(0, 0);
                if (hasScalarStorage()) {
                    return ArrayOf(dataClass(), dims, static_cast<void*>(nullptr), false);
                }
                return preserveClassType(*this,
                    ArrayOf(emptySubsetClass(dp->dataClass), dims, nullptr, isSparse(),
                        dp->fieldNames()));
            }
            if (index.isSparse()) {
                index.makeDense();
            }
            double idx = index.asDoubleScalar();
            if (!(idx >= 1) || idx != std::floor(idx)) {
                Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                    L"index must either be real positive integers or logicals.");
            }
            if (idx > static_cast<double>(elementCount())) {
                Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
            }
            return valueAtIndex(static_cast<uint64>(idx) - 1);
        }
        if (isEmpty() && index.isEmpty()) {
            // Q = ones(3,0)
            // Q(eye(2,0))
            // Q(eye(0,2))
            return preserveClassType(*this,
                ArrayOf(emptySubsetClass(dp->dataClass),
                    emptyLinearSubsetDimensions(dp->dimensions_, index.dp->dimensions_), nullptr,
                    isSparse(), dp->fieldNames()));
        }
        if (index.isEmpty()) {
            // Q = 1:10
            // Q(eye(2,0))
            // Q(eye(0,2))
            Dimensions dims = emptyLinearSubsetDimensions(dimensions(), index.dp->dimensions_);
            if (hasScalarStorage()) {
                return ArrayOf(dataClass(), dims, static_cast<void*>(nullptr), false);
            }
            return preserveClassType(*this,
                ArrayOf(
                    emptySubsetClass(dp->dataClass), dims, nullptr, isSparse(), dp->fieldNames()));
        }
        materializeScalar();
        if (index.isLogical() && !index.isSparse() && !isSparse()) {
            ArrayOf gathered;
            if (tryLogicalMaskGather(index, gathered)) {
                return gathered;
            }
        }
        if (!isSparse() && !index.isSparse()) {
            ArrayOf gathered;
            if (tryOrdinalGather(index, gathered)) {
                return gathered;
            }
        }
        index.toOrdinalType();
        Dimensions retdims = linearSubsetDimensions(index);
        if (isSparse()) {
            ArrayOf res;
            if (index.elementCount() == 1) {
                indexType indx = index.asInt32Scalar() - 1;
                indexType row = indx % dimensionLength(0);
                indexType col = indx / dimensionLength(0);
                res = ArrayOf(dp->dataClass, retdims,
                    Eigen_GetSparseScalarElement(dp->dataClass, dimensionLength(0),
                        dimensionLength(1), dp->data(), row + 1, col + 1),
                    true);
            } else {
                res = ArrayOf(dp->dataClass, retdims,
                    Eigen_GetSparseVectorSubsets(dp->dataClass, dimensionLength(0),
                        dimensionLength(1), dp->data(),
                        static_cast<const indexType*>(index.dp->data()), retdims.rows(),
                        retdims.columns()),
                    true);
            }
            return decomplexify(res);
        }
        indexType length = index.elementCount();
        const stringVector& fieldNames = dp->fieldNames();
        // every element is written below
        qp = allocateArrayOf(dp->dataClass, index.elementCount(), fieldNames, false);
        ArrayOfView<indexType> indexView(index, ordinalIndexClassForView());
        const auto* index_p = indexView.data();
        indexType bound = elementCount();
        if (isAscendingUnitStrideIndex(index_p, length)) {
            const indexType first = index_p[0] - 1;
            const indexType last = index_p[length - 1] - 1;
            if (last >= bound) {
                Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
            }
            copyElements(first, qp, 0, length);
        } else {
            for (indexType i = 0; i < length; i++) {
                indexType ndx = index_p[i] - 1;
                if (ndx < 0 || ndx >= bound) {
                    Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                }
                copyElements(ndx, qp, i, 1);
            }
        }
        ArrayOf res = ArrayOf(dp->dataClass, retdims, qp, dp->sparse, fieldNames);
        res = preserveClassType(*this, res);
        if (res.isComplex()) {
            return decomplexify(res);
        }
        return res;
    } catch (const NelsonException&) {
        deleteArrayOf(qp, dataClass());
        qp = nullptr;
        throw;
    }
}
//=============================================================================
ArrayOf
ArrayOf::nDimSubset(ArrayOfVector& index)
{
    materializeScalar();
    std::vector<constIndexPtr> indx;
    void* qp = nullptr;
    indexType i;
    try {
        indexType L = index.size();
        bool bEmpty = false;
        Dimensions dimsDest(L);
        std::vector<bool> wasColon(L, false);
        // With fewer subscripts than dimensions, the last subscript addresses the product of the
        // trailing dimensions. In column-major order this folding is contiguous, so a shape with
        // those dimensions collapsed maps to the same linear elements. When L >= ndims the
        // effective shape is the real shape.
        Dimensions effectiveDims = dp->dimensions_.foldForSubscriptCount(L);
        for (i = 0; i < L; i++) {
            if (index[i].isEmpty()) {
                bEmpty = true;
                dimsDest[i] = 0;
            } else {
                if (index[i].isRowVectorCharacterArray()) {
                    std::wstring str = index[i].asWideString();
                    if (str != L":") {
                        Error(L"Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals",
                            L"index must either be real positive integers or logicals.");
                    }
                    wasColon[i] = true;
                    indexType maxVal = effectiveDims.dimensionLength(i);
                    index[i] = ArrayOf::integerRange(1, 1, maxVal, false);
                    index[i].toOrdinalType();
                } else {
                    index[i].toOrdinalType();
                }
                ArrayOfView<indexType> indexView(index[i], ordinalIndexClassForView());
                auto* idx = indexView.data();
                if (idx != nullptr) {
                    dimsDest[i] = idx[index[i].elementCount() - 1];
                }
            }
        }
        if (bEmpty) {
            // The result is empty; each output dimension is the number of
            // selected indices along it (not the largest index value, which is
            // what dimsDest holds for the non-empty dimensions).
            // the other subscripts must still address existing elements
            Dimensions emptyDims(L);
            for (i = 0; i < L; i++) {
                emptyDims[i] = index[i].elementCount();
                if (!index[i].isEmpty()
                    && index[i].maxAsIndex() > effectiveDims.dimensionLength(i)) {
                    Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                }
            }
            return preserveClassType(*this,
                ArrayOf(emptySubsetClass(dp->dataClass), emptyDims, nullptr, isSparse(),
                    dp->fieldNames()));
        }
        indx.resize(L);
        Dimensions outDims(L);
        for (i = 0; i < L; i++) {
            outDims[i] = (index[i].elementCount());
            ArrayOfView<indexType> indexView(index[i], ordinalIndexClassForView());
            indx[i] = indexView.data();
        }
        if (outDims.elementCount() == 0) {
            for (i = 0; i < L; i++) {
                if (dimsDest[i] > effectiveDims.dimensionLength(i)) {
                    Error(L"Nelson:badsubscript", L"Index exceeds array bounds.");
                }
            }
            return preserveClassType(*this,
                ArrayOf(
                    emptySubsetClass(dp->dataClass), outDims, nullptr, false, dp->fieldNames()));
        }
        if (isSparse()) {
            if (L > 2) {
                Error(L"Nelson:types:sparseNDimIndexing",
                    L"Multidimensional indexing (more than 2 dimensions) not legal for sparse "
                    L"arrays.");
            }
            ArrayOf res;
            if ((outDims[0] == 1) && (outDims[1] == 1)) {
                res = ArrayOf(dp->dataClass, outDims,
                    Eigen_GetSparseScalarElement(dp->dataClass, dimensionLength(0),
                        dimensionLength(1), dp->data(), *(static_cast<const indexType*>(indx[0])),
                        *(static_cast<const indexType*>(indx[1]))),
                    true);
            } else {
                res = ArrayOf(dp->dataClass, outDims,
                    Eigen_GetSparseNDimSubsets(dp->dataClass, dimensionLength(0),
                        dimensionLength(1), dp->data(), static_cast<const indexType*>(indx[0]),
                        outDims[0], static_cast<const indexType*>(indx[1]), outDims[1]),
                    true);
                if (outDims[0] == 1 && wasColon[1]) {
                    res.setSparseNzmax(outDims[1]);
                }
            }
            return decomplexify(res);
        }
        const stringVector& fieldNames = dp->fieldNames();
        // every element is written below
        qp = allocateArrayOf(dp->dataClass, outDims.elementCount(), fieldNames, false);
        if (!copyContiguousBlocks(*this, effectiveDims, indx, L, outDims, qp)) {
            Dimensions argPointer(L);
            Dimensions currentIndex(L);
            indexType srcindex = 0;
            indexType dstindex = 0;
            while (argPointer.inside(outDims)) {
                for (indexType i = 0; i < L; i++) {
                    currentIndex[i] = static_cast<int>(indx[i][argPointer[i]]) - 1;
                }
                srcindex = effectiveDims.mapPoint(currentIndex);
                copyElements(srcindex, qp, dstindex, 1);
                dstindex++;
                argPointer.incrementModulo(outDims, 0);
            }
        }
        outDims.simplify();
        ArrayOf res;
        res = ArrayOf(dp->dataClass, outDims, qp, dp->sparse, fieldNames);
        res = preserveClassType(*this, res);
        if (res.isComplex()) {
            return decomplexify(res);
        }
        return res;

    } catch (const NelsonException&) {
        deleteArrayOf(qp, dp->dataClass);
        qp = nullptr;
        throw;
    }
}
//=============================================================================
static ArrayOf
preserveClassType(const ArrayOf& source, ArrayOf result)
{
    if (source.isClassType() && result.isClassType()) {
        result.setClassType(source.classType());
        if (source.dataClass() == NLS_CLASSDEF_ARRAY && result.dataClass() == NLS_CLASSDEF_ARRAY) {
            result.setClassdefLayout(source.classdefLayout());
        }
    }
    return result;
}
//=============================================================================
static Dimensions
emptyLinearSubsetDimensions(Dimensions source, Dimensions index)
{
    // Empty linear indexing of a vector (also an empty one: 1x0, 0x1) by a
    // vector index keeps the vector's orientation (row -> 1x0, column -> 0x1);
    // otherwise the result follows the index shape (A([]) is 0x0).
    bool sourceIsVector = source.length() == 2
        && ((source.dimensionLength(0) == 1) != (source.dimensionLength(1) == 1));
    bool indexIsVector
        = index.length() == 2 && (index.dimensionLength(0) == 1 || index.dimensionLength(1) == 1);
    if (sourceIsVector && indexIsVector) {
        return (source.dimensionLength(0) == 1) ? Dimensions(1, 0) : Dimensions(0, 1);
    }
    return index;
}
//=============================================================================
static NelsonType
emptySubsetClass(NelsonType dataClass)
{
    // a subset without elements has no imaginary part: it is real
    switch (dataClass) {
    case NLS_DCOMPLEX:
        return NLS_DOUBLE;
    case NLS_SCOMPLEX:
        return NLS_SINGLE;
    default:
        return dataClass;
    }
}
//=============================================================================
static bool
isAscendingUnitStrideIndex(constIndexPtr index, indexType length)
{
    if (length == 0 || index[0] == 0) {
        return false;
    }
    for (indexType i = 1; i < length; ++i) {
        if (index[i] != index[i - 1] + 1) {
            return false;
        }
    }
    return true;
}
//=============================================================================
static NelsonType
ordinalIndexClassForView()
{
#ifdef NLS_INDEX_TYPE_64
    return NLS_UINT64;
#else
    return NLS_UINT32;
#endif
}
//=============================================================================
static bool
isFullUnitStrideIndex(constIndexPtr index, indexType length, indexType sourceLength)
{
    return length == sourceLength && isAscendingUnitStrideIndex(index, length) && index[0] == 1;
}
//=============================================================================
static indexType
dimensionStride(const Dimensions& dimensions, indexType dimension)
{
    indexType stride = 1;
    for (indexType i = 0; i < dimension; i++) {
        stride *= dimensions.dimensionLength(i);
    }
    return stride;
}
//=============================================================================
static indexType
leadingFullDimensionCount(const Dimensions& sourceDims, const std::vector<constIndexPtr>& indexes,
    const Dimensions& outDims, indexType indexCount)
{
    indexType leadingFull = 0;
    for (indexType i = 0; i < indexCount; i++) {
        if (!isFullUnitStrideIndex(
                indexes[i], outDims.dimensionLength(i), sourceDims.dimensionLength(i))) {
            break;
        }
        leadingFull++;
    }
    return leadingFull;
}
//=============================================================================
static bool
allIndexesWithinBounds(const Dimensions& sourceDims, const std::vector<constIndexPtr>& indexes,
    const Dimensions& outDims, indexType firstDim, indexType indexCount)
{
    for (indexType i = firstDim; i < indexCount; i++) {
        indexType sourceLength = sourceDims.dimensionLength(i);
        indexType outputLength = outDims.dimensionLength(i);
        for (indexType j = 0; j < outputLength; j++) {
            if (indexes[i][j] == 0 || indexes[i][j] > sourceLength) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
/**
 * Copies the selection as contiguous column-major blocks. The leading ':'
 * dimensions, extended by one ascending unit-stride run (a:b) on the next
 * dimension, address a single contiguous source block for each combination of
 * the remaining subscripts: A(a:b,:), A(a:b,j), A(:,a:b,k), ...
 * A scalar leading subscript is a run of length one, so A(k,:) takes the block
 * loop too. Returns false (nothing copied) when the first subscript is neither
 * ':' nor an ascending run, or when a subscript is out of bounds; the caller
 * then runs the element-wise loop, which also owns the error reporting.
 */
static bool
copyContiguousBlocks(ArrayOf& source, const Dimensions& sourceDims,
    const std::vector<constIndexPtr>& indexes, indexType indexCount, const Dimensions& outDims,
    void* destination)
{
    indexType blockDim = leadingFullDimensionCount(sourceDims, indexes, outDims, indexCount);
    indexType blockSize = dimensionStride(sourceDims, blockDim);
    indexType blockOffset = 0;
    if (blockDim < indexCount) {
        indexType runLength = outDims.dimensionLength(blockDim);
        constIndexPtr run = indexes[blockDim];
        if (isAscendingUnitStrideIndex(run, runLength)) {
            if (run[runLength - 1] > sourceDims.dimensionLength(blockDim)) {
                return false;
            }
            blockOffset = (run[0] - 1) * blockSize;
            blockSize *= runLength;
            blockDim++;
        }
    }
    if (blockDim == 0
        || !allIndexesWithinBounds(sourceDims, indexes, outDims, blockDim, indexCount)) {
        return false;
    }
    if (blockDim >= indexCount) {
        source.copyElements(blockOffset, destination, 0, blockSize);
        return true;
    }
    indexType outerCount = indexCount - blockDim;
    indexType strides[maxDims];
    Dimensions blockDims(outerCount);
    for (indexType i = 0; i < outerCount; i++) {
        strides[i] = dimensionStride(sourceDims, blockDim + i);
        blockDims[i] = outDims.dimensionLength(blockDim + i);
    }
    Dimensions blockPointer(outerCount);
    indexType dstIndex = 0;
    while (blockPointer.inside(blockDims)) {
        indexType srcIndex = blockOffset;
        for (indexType i = 0; i < outerCount; i++) {
            srcIndex += (indexes[blockDim + i][blockPointer[i]] - 1) * strides[i];
        }
        source.copyElements(srcIndex, destination, dstIndex, blockSize);
        dstIndex += blockSize;
        blockPointer.incrementModulo(blockDims, 0);
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
