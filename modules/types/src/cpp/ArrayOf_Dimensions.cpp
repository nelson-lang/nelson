//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4724)
#endif
//=============================================================================
#include <cwchar>
#include <cstring>
#include "lapack_eigen_config.hpp"
#include "omp_for_loop.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <algorithm>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <limits>
#include "StringHelpers.hpp"
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleState.hpp"
#include "IEEEFP.hpp"
#include "SparseType.hpp"
#include "characters_encoding.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "NelsonException.hpp"
#include "NewWithException.hpp"
//=============================================================================
#ifdef _MSC_VER
#define snprintf _snprintf
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
indexType
ArrayOf::rows() const
{
    if (hasScalarStorage()) {
        return 1;
    }
    if (dp) {
        return dp->rows();
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::columns() const
{
    if (hasScalarStorage()) {
        return 1;
    }
    if (dp) {
        return dp->columns();
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::nDims() const
{
    if (hasScalarStorage()) {
        return scalarDimensionCount_;
    }
    if (dp) {
        return dp->dimensions_.length();
    }
    return 0;
}
//=============================================================================
indexType
ArrayOf::elementCount() const
{
    if (hasScalarStorage()) {
        return 1;
    }
    if (dp) {
        return dp->elementCount();
    }
    return 0;
}
//=============================================================================
Dimensions
ArrayOf::dimensions() const
{
    if (hasScalarStorage()) {
        Dimensions dims(scalarDimensionCount_);
        for (indexType k = 0; k < scalarDimensionCount_; ++k) {
            dims[k] = 1;
        }
        return dims;
    }
    if (dp) {
        return dp->dimensions_;
    }
    return {};
}
//=============================================================================
indexType
ArrayOf::dimensionLength(int t) const
{
    if (hasScalarStorage()) {
        Dimensions scalarDimensions(1, 1);
        return scalarDimensions[t];
    }
    if (dp) {
        return dp->dimensions_[t];
    }
    return 0;
}
//=============================================================================
void
ArrayOf::resize(Dimensions& a)
{
    materializeScalar();
    Dimensions newSize;
    newSize = dp->dimensions_;
    newSize.expandToCover(a);
    if (newSize.equals(dp->dimensions_)) {
        return;
    }
    if (newSize.elementCount() == elementCount()) {
        ensureSingleOwner();
        dp->dimensions_ = newSize;
        dp->refreshDimensionCache();
        return;
    }
    if (isSparse()) {
        Error(L"Nelson:types:sparseNotSupported:cannotResizeSparseArrays",
            L"Cannot resize sparse arrays.");
    }
    const stringVector& fieldNames = dp->fieldNames();
    void* dst_data = allocateArrayOf(dp->dataClass, newSize.elementCount(), fieldNames, true);
    if (!isEmpty()) {
        Dimensions curPos(dp->dimensions_.length());
        indexType rowCount = dp->dimensions_[0];
        indexType srcIndex = 0;
        while (curPos.inside(dp->dimensions_)) {
            indexType dstIndex = newSize.mapPoint(curPos);
            copyElements(srcIndex, dst_data, dstIndex, rowCount);
            curPos.incrementModulo(dp->dimensions_, 1);
            srcIndex += rowCount;
        }
    }
    dp = dp->putData(dp->dataClass, newSize, dst_data, dp->sparse, fieldNames);
}
//=============================================================================
static bool
canUseVectorCapacity(NelsonType dataClass)
{
    // sparse storage is rejected by the caller; handle-like slots keep the
    // reallocating path, every other type is element-major (allocateArrayOf
    // counts length * fieldNames and copyElements applies the same stride,
    // so spare capacity slots behave like the reallocating path)
    switch (dataClass) {
    case NLS_FUNCTION_HANDLE:
    case NLS_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
        return false;
    default:
        return true;
    }
}
//=============================================================================
static indexType
nextVectorCapacity(indexType currentCount, indexType requestedCount)
{
    indexType capacity = currentCount < 16 ? 16 : currentCount + currentCount / 2;
    if (capacity < requestedCount) {
        capacity = requestedCount;
    }
    return capacity;
}
//=============================================================================
static Dimensions
emptyVectorGrowthDimensions(const Dimensions& dims, indexType max_index)
{
    // An empty array grown through a linear index becomes a row, except a
    // 0x1 column, or an N-D empty whose only non-singleton dimension is the
    // zero-length one: it grows along that dimension.
    Dimensions newDim;
    if (dims.is2D()) {
        if (dims.at(0, false) == 0 && dims.at(1, false) == 1) {
            newDim[0] = max_index;
            newDim[1] = 1;
        } else {
            newDim[0] = 1;
            newDim[1] = max_index;
        }
        return newDim;
    }
    indexType zeroDim = dims.length();
    for (indexType k = 0; k < dims.length(); ++k) {
        const indexType len = dims.at(k, false);
        if (len == 0 && zeroDim == dims.length()) {
            zeroDim = k;
        } else if (len != 1) {
            Error(L"Nelson:types:invalidDimensions:attemptGrowArrayAlongAmbiguousDimension",
                L"Attempt to grow array along ambiguous dimension.");
        }
    }
    newDim = dims;
    newDim[zeroDim] = max_index;
    return newDim;
}
//=============================================================================
void
ArrayOf::vectorResize(indexType max_index)
{
    materializeScalar();
    if (max_index > elementCount()) {
        Dimensions newDim;
        if (isEmpty()) {
            newDim = emptyVectorGrowthDimensions(dp->dimensions_, max_index);
        } else if (dp->isScalar()) {
            newDim.reset();
            newDim[0] = 1;
            newDim[1] = max_index;
        } else if (dp->isVector()) {
            newDim = dp->dimensions_;
            if (dp->dimensions_[0] != 1) {
                newDim[0] = max_index;
            } else {
                newDim[1] = max_index;
            }
        } else {
            Error(L"Nelson:types:invalidDimensions:attemptGrowArrayAlongAmbiguousDimension",
                L"Attempt to grow array along ambiguous dimension.");
        }
        if (!isSparse() && canUseVectorCapacity(dp->dataClass)) {
            ensureSingleOwner();
            if (max_index <= dp->allocatedElementCount) {
                dp->dimensions_ = newDim;
                dp->refreshDimensionCache();
                return;
            }
            const indexType currentCount = elementCount();
            const indexType capacity = nextVectorCapacity(currentCount, max_index);
            const stringVector& fieldNames = dp->fieldNames();
            void* dstData = allocateArrayOf(dp->dataClass, capacity, fieldNames, true);
            if (currentCount > 0) {
                copyElements(0, dstData, 0, currentCount);
            }
            dp = dp->putData(dp->dataClass, newDim, dstData, dp->sparse, fieldNames, capacity);
            return;
        }
        resize(newDim);
    }
}
//=============================================================================
/**
 * Reshape an array.  This is only legal if the number of
 * elements remains the same after reshaping.
 */
void
ArrayOf::reshape(Dimensions& a, bool checkValidDimension)
{
    materializeScalar();
    if (isFunctionHandle()) {
        Error(L"Nelson:types:invalidDimensions:valueOperationNotAllowedFunctionHandle",
            L"%s operation not allowed for 'function_handle' type.", L"Reshape");
    }
    if (checkValidDimension) {
        if (a.elementCount() != elementCount()) {
            Error(L"Nelson:types:invalidDimensions:valueOperationCannotChangeNumberElements",
                L"%s operation cannot change the number of elements in array.", L"Reshape");
        }
    }
    if (isSparse()) {
        if (a.is2D() || a.isVector() || a.isScalar()) {
            void* reshapedSparseMatrix = Eigen_ReshapeSparseMatrix(
                dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], a[0], a[1], dp->data());
            dp = dp->putData(dp->dataClass, a, reshapedSparseMatrix, true);
            dp->dimensions_ = a;
            dp->refreshDimensionCache();
        } else {
            Error(L"Nelson:types:sparseNotSupported:reshapeOperationNotAllowedNDimensions",
                L"Reshape operation not allowed with N Dimensions sparse arrays.");
        }
    } else {
        ensureSingleOwner();
        dp->dimensions_ = a;
        dp->refreshDimensionCache();
    }
}
//=============================================================================
void
ArrayOf::changeInPlaceDimensions(const Dimensions& a)
{
    materializeScalar();
    if (isClassType()) {
        Error(L"Nelson:types:invalidDimensions:changeDimensionsOperationNotAllowedOverloadedType",
            L"changeDimensions operation not allowed for overloaded type.");
    }
    if (isFunctionHandle()) {
        Error(L"Nelson:types:invalidDimensions:valueOperationNotAllowedFunctionHandle",
            L"%s operation not allowed for 'function_handle' type.", L"changeDimensions");
    }
    if (a.elementCount() != elementCount()) {
        Error(L"Nelson:types:invalidDimensions:valueOperationCannotChangeNumberElements",
            L"%s operation cannot change the number of elements in array.", L"changeDimensions");
    }
    dp->dimensions_ = a;
    dp->refreshDimensionCache();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
