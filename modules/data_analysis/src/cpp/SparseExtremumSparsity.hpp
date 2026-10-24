//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// C = max(A, B) or min(A, B) with a sparse operand stays sparse only when it is
// sparsity preserving: no dense matrix operand, not two scalars, same size when
// both are matrices, and a zero background (the extremum of the fill values,
// where a scalar fills with its value and a matrix with its implicit zeros).
inline bool
sparseExtremumStaysSparse(const ArrayOf& A, const ArrayOf& B, bool backgroundIsZero)
{
    bool aScalar = A.isScalar();
    bool bScalar = B.isScalar();
    if ((!A.isSparse() && !aScalar) || (!B.isSparse() && !bScalar) || (aScalar && bScalar)
        || !backgroundIsZero) {
        return false;
    }
    return aScalar || bScalar || A.dimensions().equals(B.dimensions());
}
//=============================================================================
// Dense scalar fill value of an operand: the scalar itself or an implicit zero.
inline ArrayOf
sparseExtremumFill(const ArrayOf& X)
{
    ArrayOf fill = X.isScalar() ? X : ArrayOf::doubleScalar(0);
    if (fill.isSparse()) {
        fill.makeDense();
    }
    fill.promoteType(X.dataClass());
    return fill;
}
//=============================================================================
inline bool
sparseExtremumIsZero(const ArrayOf& scalar)
{
    ArrayOf value(scalar);
    value.promoteType(NLS_DCOMPLEX);
    const double* ptr = (const double*)value.dataPointer();
    return ptr[0] == 0 && ptr[1] == 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
