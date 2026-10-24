//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "LapackLeastSquaresSolver.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
ArrayOf
solveSVDDecompositionReal(NelsonType destinationClass, const ArrayOf& matA, const ArrayOf& matB)
{
    lapack_int rank = 0;
    lapack_int info = 0;
    ArrayOf res = solveLapackLeastSquares<T>(destinationClass, matA, matB, true, rank, info);
    if (info != 0) {
        Error(L"Nelson:operators:svdSolverFailed", L"SVD least squares solver failed.");
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
