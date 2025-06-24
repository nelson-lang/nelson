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
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isMissing() const
{
    if (dp) {
        return (dp->dataClass == NLS_MISSING_ARRAY);
    }
    return false;
}
//=============================================================================
bool
ArrayOf::isNdArrayMissing() const
{
    if (dp) {
        return (dp->dataClass == NLS_MISSING_ARRAY) && !is2D();
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::missingScalarConstructor()
{
    Dimensions dim;
    dim.makeScalar();
    double* data
        = static_cast<double*>(allocateArrayOf(NLS_MISSING_ARRAY, 1, stringVector(), false));
    *data = std::nan("NaN");
    return ArrayOf(NLS_MISSING_ARRAY, dim, data);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
