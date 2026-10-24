//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cholBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "CholeskyFactorization.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Dense>
#include <Eigen/SparseCholesky>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static int
sparseCholeskyFailureIndex(const Eigen::SparseMatrix<T, 0, signedIndexType>& matrix)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using DenseMatrix = Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>;
    const int order = static_cast<int>(matrix.rows());
    for (int k = 1; k <= order; ++k) {
        DenseMatrix leading = DenseMatrix::Zero(k, k);
        for (int outer = 0; outer < matrix.outerSize(); ++outer) {
            for (typename SparseMatrix::InnerIterator it(matrix, outer); it; ++it) {
                if (it.row() < k && it.col() < k) {
                    leading(it.row(), it.col()) = it.value();
                }
            }
        }
        Eigen::LLT<DenseMatrix> llt(leading);
        if (llt.info() != Eigen::Success) {
            return k;
        }
    }
    return order;
}
//=============================================================================
template <class T>
static ArrayOfVector
sparseCholeskyFactorization(const ArrayOf& A, int nLhs)
{
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* spmat = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    Eigen::SimplicialLLT<SparseMatrix, Eigen::Upper, Eigen::NaturalOrdering<signedIndexType>>
        solver;
    solver.compute(*spmat);
    int p = solver.info() == Eigen::Success ? 0 : sparseCholeskyFailureIndex<T>(*spmat);
    if (p != 0 && nLhs < 2) {
        Error(L"Nelson:chol:positiveFiniteMatrixExpected", L"%s expected.",
            L"Positive finite matrix");
    }

    SparseMatrix R;
    if (p == 0) {
        R = solver.matrixU();
        R.makeCompressed();
    } else {
        R.resize(A.rows(), A.columns());
    }

    ArrayOfVector retval;
    retval << makeSparseResult<T>(A.dataClass(), A.dimensions(), new SparseMatrix(R));
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(p);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::cholBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    if (param1.isSparse()) {
        switch (param1.dataClass()) {
        case NLS_DOUBLE:
            return sparseCholeskyFactorization<double>(param1, nLhs);
        case NLS_DCOMPLEX:
            return sparseCholeskyFactorization<doublecomplex>(param1, nLhs);
        case NLS_SINGLE:
            return sparseCholeskyFactorization<single>(param1, nLhs);
        case NLS_SCOMPLEX:
            return sparseCholeskyFactorization<singlecomplex>(param1, nLhs);
        default:
            OverloadRequired("chol");
        }
    }
    int p = 0;
    ArrayOf res = CholeskyFactorization(param1, false, needToOverload, p);
    if (needToOverload) {
        OverloadRequired("chol");
    } else {
        if (p != 0 && nLhs < 2) {
            Error(L"Nelson:chol:positiveFiniteMatrixExpected", L"%s expected.",
                L"Positive finite matrix");
        }
        retval << res;
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar(p);
        }
    }
    return retval;
}
//=============================================================================
