//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "Error.hpp"
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
    auto* qp = (logical*)dp->getData();
    return (*qp);
}
} // namespace Nelson
//=============================================================================
