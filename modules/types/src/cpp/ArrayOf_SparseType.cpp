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
#include "SparseDynamicFunctions.hpp"
#include "Error.hpp"
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
    if (dp->sparse) {
        return dp->dataClass == NLS_DOUBLE || (dp->dataClass == NLS_DCOMPLEX && !realOnly);
    }
    return false;
}
//=============================================================================
const void*
ArrayOf::getSparseDataPointer() const
{
    if (dp) {
        return dp->getData();
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
        dp->sparse = false;
        return;
    }
    ensureSingleOwner();
    dp = dp->putData(dp->dataClass, dp->dimensions,
        MakeDenseArrayOfDynamicFunction(
            dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
        false, dp->fieldNames);
}
//=============================================================================
indexType
ArrayOf::getNonzeros() const
{
    if (!isSparse()) {
        return (dp->getElementCount());
    }
    if (isEmpty()) {
        return 0;
    }
    return CountNonzerosDynamicFunction(
        dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData());
}
//=============================================================================
void
ArrayOf::makeSparse()
{
    if ((dp->dataClass == NLS_DOUBLE) || (dp->dataClass == NLS_DCOMPLEX)
        || (dp->dataClass == NLS_LOGICAL)) {
        ensureSingleOwner();
    } else {
        Error(_W("Cannot make sparse."));
    }
    if (isSparse()) {
        return;
    }
    if (isReferenceType() || isCharacterArray()) {
        Error(_W("Cannot make strings or reference types sparse."));
    }
    if (!is2D()) {
        Error(_W("Cannot make n-dimensional arrays sparse."));
    }
    if (isEmpty()) {
        void* cp = ArrayOf::allocateArrayOf(dp->dataClass, 0, stringVector(), true);
        dp = dp->putData(dp->dataClass, dp->dimensions,
            MakeSparseArrayOfDynamicFunction(
                dp->dataClass, dp->dimensions[0], dp->dimensions[1], cp),
            true, dp->fieldNames);
        return;
    }
    dp = dp->putData(dp->dataClass, dp->dimensions,
        MakeSparseArrayOfDynamicFunction(
            dp->dataClass, dp->dimensions[0], dp->dimensions[1], dp->getData()),
        true, dp->fieldNames);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
