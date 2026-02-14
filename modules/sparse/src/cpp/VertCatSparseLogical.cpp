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
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
VertCatSparseLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if (!A.isSparseLogicalType()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 1, NLS_SPARSE_LOGICAL_STR);
    }
    if (!B.isSparseLogicalType()) {
        raiseError2(L"Nelson:error_manager:wrong_type_with_expected", 2, NLS_SPARSE_LOGICAL_STR);
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
