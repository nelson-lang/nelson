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
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isLogical() const
{
    if (hasScalarStorage()) {
        return scalarType_ == NLS_LOGICAL;
    }
    return dp && dp->dataClass == NLS_LOGICAL;
}
//=============================================================================
bool
ArrayOf::isNdArrayLogical() const
{
    return isLogical() && !is2D();
}
//=============================================================================
bool
ArrayOf::isSparseLogicalType() const
{
    return dp && dp->dataClass == NLS_LOGICAL && dp->sparse && is2D();
}
//=============================================================================
ArrayOf
ArrayOf::logicalScalar(bool aval)
{
    return ArrayOf(Representation::InlineScalar, static_cast<logical>(aval));
}
//=============================================================================
logical
ArrayOf::asLogicalScalar(bool arrayAsScalar) const
{
    if (!isLogical()) {
        Error(L"Nelson:types:logicalExpected", L"logical expected.");
    }
    if (isEmpty() || (!arrayAsScalar && !isScalar())) {
        Error(L"Nelson:types:scalarExpected", L"A scalar expected.");
    }
    if (!isSparse()) {
        auto* qp = (const logical*)dataPointer();
        return (*qp);
    }
    Eigen::SparseMatrix<logical, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<logical, 0, signedIndexType>*)sparseDataPointer();
    spMat->finalize();
    return spMat->coeff(0, 0);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
