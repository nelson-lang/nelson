//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "gmresBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseAdvancedHelpers.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T> struct GmresResult
{
    SparseAdvancedVector<T> x;
    double flag = 1;
    double relres = 0;
    std::vector<double> iter = { 0, 0 };
    std::vector<double> resvec;
};
//=============================================================================
inline indexType
optionalRestart(const ArrayOfVector& argIn, size_t index, indexType matrixSize)
{
    indexType restart = optionalIterationCount(argIn, index, matrixSize);
    if (restart == 0) {
        Error(L"Nelson:gmres:invalidRestart", L"%s must be a positive integer scalar.", L"Restart");
    }
    return std::min<indexType>(restart, matrixSize);
}
//=============================================================================
template <class T>
double
preconditionedResidualNorm(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b,
    const SparseAdvancedVector<T>& x, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2)
{
    return applyPreconditioners<T>(M1, M2, b - A * x).norm();
}
//=============================================================================
template <class T>
GmresResult<T>
runGmres(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, indexType restart,
    double tolerance, indexType maxOuterIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    validateSystem<T>(A, b);
    GmresResult<T> result;
    result.x = x0;
    double normb = b.norm();
    if (normb == 0) {
        normb = 1;
    }

    double trueResidual = (b - A * result.x).norm();
    result.relres = trueResidual / normb;
    result.resvec.push_back(preconditionedResidualNorm<T>(A, b, result.x, M1, M2));
    if (result.relres <= tolerance) {
        result.flag = 0;
        return result;
    }
    if (maxOuterIterations == 0) {
        result.iter = { 1, 0 };
        return result;
    }

    for (indexType outer = 1; outer <= maxOuterIterations; ++outer) {
        SparseAdvancedVector<T> r = applyPreconditioners<T>(M1, M2, b - A * result.x);
        double beta = r.norm();
        if (beta == 0) {
            result.flag = 0;
            result.relres = 0;
            result.iter = { static_cast<double>(outer - 1), 0 };
            return result;
        }

        DenseAdvancedMatrix<T> V(b.size(), restart + 1);
        DenseAdvancedMatrix<T> H = DenseAdvancedMatrix<T>::Zero(restart + 1, restart);
        V.col(0) = r / beta;

        for (indexType inner = 1; inner <= restart; ++inner) {
            SparseAdvancedVector<T> w = applyPreconditioners<T>(
                M1, M2, A * V.col(static_cast<signedIndexType>(inner - 1)));
            for (indexType k = 0; k < inner; ++k) {
                H(static_cast<signedIndexType>(k), static_cast<signedIndexType>(inner - 1))
                    = V.col(static_cast<signedIndexType>(k)).dot(w);
                w -= H(static_cast<signedIndexType>(k), static_cast<signedIndexType>(inner - 1))
                    * V.col(static_cast<signedIndexType>(k));
            }
            double hNext = w.norm();
            H(static_cast<signedIndexType>(inner), static_cast<signedIndexType>(inner - 1))
                = realValueToScalar<T>(hNext);
            if (hNext != 0 && inner < restart) {
                V.col(static_cast<signedIndexType>(inner)) = w / hNext;
            }

            Eigen::Matrix<T, Eigen::Dynamic, 1> g(inner + 1);
            g.setZero();
            g(0) = realValueToScalar<T>(beta);
            auto hBlock = H.block(0, 0, inner + 1, inner);
            Eigen::Matrix<T, Eigen::Dynamic, 1> y = hBlock.colPivHouseholderQr().solve(g);
            SparseAdvancedVector<T> candidate = result.x + V.block(0, 0, b.size(), inner) * y;

            result.resvec.push_back(preconditionedResidualNorm<T>(A, b, candidate, M1, M2));
            trueResidual = (b - A * candidate).norm();
            result.relres = trueResidual / normb;
            result.iter = { static_cast<double>(outer), static_cast<double>(inner) };
            if (result.relres <= tolerance) {
                result.x = candidate;
                result.flag = 0;
                return result;
            }
            if (hNext == 0) {
                result.x = candidate;
                result.flag = 4;
                return result;
            }
            if (inner == restart) {
                result.x = candidate;
            }
        }
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
gmresTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("gmres");
    }
    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedVector<T> b = arrayToVector<T>(barg);
    indexType restart = optionalRestart(argIn, 2, A.rows());
    double tolerance = optionalTolerance(argIn, 3, 1e-6);
    indexType maxOuterIterations
        = optionalIterationCount(argIn, 4, std::min<indexType>(A.rows(), 10));
    ExplicitPreconditioner<T> M1;
    ExplicitPreconditioner<T> M2;
    if (argIn.size() > 5) {
        M1.compute(argIn[5], true, b.size(), eval);
    }
    if (argIn.size() > 6) {
        M2.compute(argIn[6], false, b.size(), eval);
    }
    SparseAdvancedVector<T> x0(b.size());
    x0.setZero();
    if (argIn.size() > 7 && !argIn[7].isEmpty()) {
        x0 = arrayToVector<T>(argIn[7]);
    }
    if (x0.size() != b.size()) {
        Error(L"Nelson:linear_algebra:initialGuessDimensionMismatch",
            L"Initial guess dimensions must agree.");
    }

    GmresResult<T> result = runGmres<T>(A, b, restart, tolerance, maxOuterIterations, M1, M2, x0);
    ArrayOfVector retval;
    retval << vectorToArrayOf<T>(dataClass, barg.dimensions(), result.x);
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(result.flag);
    }
    if (nLhs > 2) {
        retval << ArrayOf::doubleScalar(result.relres);
    }
    if (nLhs > 3) {
        retval << doubleRowVector(result.iter);
    }
    if (nLhs > 4) {
        retval << doubleColumnVector(result.resvec);
    }
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::gmresBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 8);
    nargoutcheck(nLhs, 0, 5);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("gmres");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 5, 6, 7 }, NLS_SCOMPLEX)) {
        return gmresTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 5, 6, 7 }, NLS_DCOMPLEX)) {
        return gmresTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return gmresTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return gmresTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("gmres");
    return {};
}
//=============================================================================
