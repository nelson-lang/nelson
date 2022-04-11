//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NaN.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
NaN()
{
    return NaN(1, 1);
}
//=============================================================================
ArrayOf
NaN(indexType m, indexType n)
{
    double* mat = static_cast<double*>(ArrayOf::allocateArrayOf(
        NLS_DOUBLE, static_cast<indexType>(m * n), Nelson::stringVector(), false));
    Eigen::Map<Eigen::MatrixXd> matNaN(mat, m, n);
    matNaN.setConstant(std::nan("NaN"));
    Dimensions dimMat(m, n);
    ArrayOf res = ArrayOf(NLS_DOUBLE, dimMat, mat);
    return res;
}

} // namespace Nelson
//=============================================================================
