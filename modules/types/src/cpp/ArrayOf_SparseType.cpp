//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Sparse>
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "SparseType.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isSparse() const
{
    if (dp == nullptr) {
        return false;
    }
    return (dp->sparse);
}
//=============================================================================
bool
ArrayOf::isSparseDoubleType(bool realOnly) const
{
    if (hasScalarStorage()) {
        return false;
    }
    if (dp->sparse) {
        return dp->dataClass == NLS_DOUBLE || (dp->dataClass == NLS_DCOMPLEX && !realOnly);
    }
    return false;
}
//=============================================================================
const void*
ArrayOf::sparseDataPointer() const
{
    if (hasScalarStorage()) {
        return nullptr;
    }
    if (dp) {
        return dp->data();
    }
    return nullptr;
}
//=============================================================================
void*
ArrayOf::sparseReadWriteDataPointer()
{
    if (!isSparse()) {
        Error(L"Nelson:types:sparseNotSupported:operationRequiresSparseMatrixArguments",
            L"operation requires sparse matrix arguments.");
    }
    ensureSingleOwner();
    if (dp) {
        return dp->writableData();
    }
    return nullptr;
}
//=============================================================================
void
ArrayOf::makeDense()
{
    if (!isSparse()) {
        return;
    }
    if (isEmpty()) {
        ensureSingleOwner();
        dp->sparse = false;
        return;
    }
    ensureSingleOwner();
    replaceData(dp->dataClass, dp->dimensions_,
        Eigen_MakeDenseArrayOf(dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], dp->data()),
        false, dp->fieldNames_);
}
//=============================================================================
indexType
ArrayOf::nonzeros() const
{
    if (!isSparse()) {
        return elementCount();
    }
    if (isEmpty()) {
        return 0;
    }
    return Eigen_CountNonzeros(dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], dp->data());
}
//=============================================================================
void
ArrayOf::makeSparse()
{
    materializeScalar();
    if ((dp->dataClass == NLS_DOUBLE) || (dp->dataClass == NLS_DCOMPLEX)
        || (dp->dataClass == NLS_SINGLE) || (dp->dataClass == NLS_SCOMPLEX)
        || (dp->dataClass == NLS_LOGICAL)) {
        ensureSingleOwner();
    } else {
        Error(L"Nelson:types:sparseNotSupported:cannotMakeSparse", L"Cannot make sparse.");
    }
    if (isSparse()) {
        return;
    }
    if (isReferenceType() || isCharacterArray()) {
        Error(L"Nelson:types:sparseNotSupported:cannotMakeStringsReferenceTypesSparse",
            L"Cannot make strings or reference types sparse.");
    }
    if (!is2D()) {
        Error(L"Nelson:types:sparseNotSupported:cannotMakeNDimensionalArraysSparse",
            L"Cannot make n-dimensional arrays sparse.");
    }
    if (isEmpty()) {
        void* cp = ArrayOf::allocateArrayOf(dp->dataClass, 0, stringVector(), true);
        dp = dp->putData(dp->dataClass, dp->dimensions_,
            Eigen_MakeSparseArrayOf(dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], cp),
            true, dp->fieldNames_);
        return;
    }
    dp = dp->putData(dp->dataClass, dp->dimensions_,
        Eigen_MakeSparseArrayOf(dp->dataClass, dp->dimensions_[0], dp->dimensions_[1], dp->data()),
        true, dp->fieldNames_);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
