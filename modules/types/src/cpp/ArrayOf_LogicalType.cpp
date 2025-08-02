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
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isLogical() const
{
    if (dp) {
        return (dp->dataClass == NLS_LOGICAL);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isNdArrayLogical() const
{
    if (dp) {
        return (dp->dataClass == NLS_LOGICAL) && !is2D();
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isSparseLogicalType() const
{
    if (dp) {
        return (dp->dataClass == NLS_LOGICAL) && (dp->sparse) && is2D();
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::logicalConstructor(bool aval)
{
    Dimensions dim;
    dim.makeScalar();
    logical* data = static_cast<logical*>(allocateArrayOf(NLS_LOGICAL, 1, stringVector(), false));
    *data = static_cast<logical>(aval);
    return ArrayOf(NLS_LOGICAL, dim, data);
}
//=============================================================================
logical
ArrayOf::getContentAsLogicalScalar(bool arrayAsScalar) const
{
    if (!isLogical()) {
        Error(ERROR_TYPE_LOGICAL_EXPECTED);
    }
    if (isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(ERROR_SIZE_SCALAR_EXPECTED);
    }
    if (!isSparse()) {
        auto* qp = (logical*)dp->getData();
        return (*qp);
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)getSparseDataPointer();
    spMat->finalize();
    return spMat->coeff(0, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
