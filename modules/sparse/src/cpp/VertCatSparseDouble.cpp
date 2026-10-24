//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "VertCatSparseDouble.hpp"
#include "CtransposeSparseDouble.hpp"
#include "HorzCatSparseDouble.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "ConcatenateDimensions.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isSparseFloatingType(const ArrayOf& value)
{
    if (!value.isSparse()) {
        return false;
    }
    NelsonType dataClass = value.dataClass();
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX || dataClass == NLS_SINGLE
        || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
ArrayOf
VertCatSparseDouble(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!isSparseFloatingType(A)) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse double");
    }
    if (!isSparseFloatingType(B)) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
            L"sparse double");
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    if (dimsA.columns() != dimsB.columns()) {
        raiseConcatenationDimensionMismatch();
    }
    A = CtransposeSparseDouble(A);
    B = CtransposeSparseDouble(B);
    C = HorzCatSparseDouble(A, B);
    C = CtransposeSparseDouble(C);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
