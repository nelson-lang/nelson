//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Inf.hpp"
#include <Eigen/Dense>
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Inf()
{
    return Inf(1, 1);
}
//=============================================================================
ArrayOf
Inf(uint32 m, uint32 n)
{
    double* mat = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE,
        static_cast<indexType>(m) * static_cast<indexType>(n), Nelson::stringVector(), false));
    Eigen::Map<Eigen::MatrixXd> matInf(mat, m, n);
    matInf.setConstant(std::numeric_limits<double>::infinity());
    Dimensions dimMat(m, n);
    ArrayOf res = ArrayOf(NLS_DOUBLE, dimMat, mat);
    return res;
}

} // namespace Nelson
//=============================================================================