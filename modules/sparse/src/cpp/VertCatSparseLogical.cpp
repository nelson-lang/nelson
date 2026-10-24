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
#include "VertCatSparseLogical.hpp"
#include "HorzCatSparseLogical.hpp"
#include "CtransposeSparseLogical.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"sparse logical");
    }
    if (!B.isSparseLogicalType()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
            L"sparse logical");
    }
    if (A.isEmpty(false)) {
        ArrayOf C(B);
        return C;
    }
    if (B.isEmpty(false)) {
        ArrayOf C(A);
        return C;
    }
    A = CtransposeSparseLogical(A);
    B = CtransposeSparseLogical(B);
    C = HorzCatSparseLogical(A, B);
    C = CtransposeSparseLogical(C);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
