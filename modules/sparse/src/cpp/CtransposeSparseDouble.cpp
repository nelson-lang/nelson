//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CtransposeSparseDouble.hpp"
#include "lapack_eigen_config.hpp"
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
CtransposeSparseDouble(const ArrayOf& A)
{
    ArrayOf C;
    if (A.isEmpty()) {
        Dimensions dimsC(A.getColumns(), A.getRows());
        C = ArrayOf(NLS_DOUBLE, dimsC, (void*)nullptr, true);
    } else {
        if (A.isComplex()) {
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatA
                = (Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>*)A.getSparseDataPointer();
            Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>* spMatC;
            try {
                spMatC = new Eigen::SparseMatrix<doublecomplex, 0, signedIndexType>(
                    spMatA->cols(), spMatA->rows());
            } catch (const std::bad_alloc&) {
                spMatC = nullptr;
                Error(ERROR_MEMORY_ALLOCATION);
            }
            *spMatC = spMatA->adjoint();
            Dimensions dimsC = Dimensions(spMatC->rows(), spMatC->cols());
            C = ArrayOf(NLS_DCOMPLEX, dimsC, (void*)spMatC, true);
        } else {
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMatA
                = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
            Eigen::SparseMatrix<double, 0, signedIndexType>* spMatC;
            try {
                spMatC = new Eigen::SparseMatrix<double, 0, signedIndexType>(
                    spMatA->cols(), spMatA->rows());
            } catch (const std::bad_alloc&) {
                spMatC = nullptr;
                Error(ERROR_MEMORY_ALLOCATION);
            }
            *spMatC = spMatA->adjoint();
            Dimensions dimsC = Dimensions(spMatC->rows(), spMatC->cols());
            C = ArrayOf(NLS_DOUBLE, dimsC, (void*)spMatC, true);
        }
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
