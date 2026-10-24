//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "minresBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseAdvancedHelpers.hpp"
#include "ErrorID.hpp"
#include <algorithm>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T> struct MinresResult
{
    SparseAdvancedVector<T> x;
    double flag = 1;
    double relres = 0;
    double iter = 0;
    std::vector<double> resvec;
};
//=============================================================================
template <class T>
MinresResult<T>
runMinres(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    validateSystem<T>(A, b);
    MinresResult<T> result;
    result.x = x0;
    double normb = b.norm();
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> r = b - A * result.x;
    double residualNorm = r.norm();
    result.resvec.push_back(residualNorm);
    result.relres = residualNorm / normb;
    if (result.relres <= tolerance) {
        result.flag = 0;
        return result;
    }
    SparseAdvancedVector<T> preconditionedResidual = applyPreconditioners<T>(M1, M2, r);
    double beta = preconditionedResidual.norm();
    if (beta == 0) {
        result.flag = 4;
        return result;
    }

    indexType effectiveMaxIterations = std::max<indexType>(maxIterations, 1);
    DenseAdvancedMatrix<T> V(b.size(), effectiveMaxIterations + 1);
    DenseAdvancedMatrix<T> H
        = DenseAdvancedMatrix<T>::Zero(effectiveMaxIterations + 1, effectiveMaxIterations);
    V.col(0) = preconditionedResidual / beta;
    for (indexType k = 1; k <= effectiveMaxIterations; ++k) {
        SparseAdvancedVector<T> w
            = applyPreconditioners<T>(M1, M2, A * V.col(static_cast<signedIndexType>(k - 1)));
        for (indexType j = 0; j < k; ++j) {
            H(static_cast<signedIndexType>(j), static_cast<signedIndexType>(k - 1))
                = V.col(static_cast<signedIndexType>(j)).dot(w);
            w -= H(static_cast<signedIndexType>(j), static_cast<signedIndexType>(k - 1))
                * V.col(static_cast<signedIndexType>(j));
        }
        double hNext = w.norm();
        H(static_cast<signedIndexType>(k), static_cast<signedIndexType>(k - 1))
            = realValueToScalar<T>(hNext);
        if (hNext != 0 && k < effectiveMaxIterations) {
            V.col(static_cast<signedIndexType>(k)) = w / hNext;
        }

        Eigen::Matrix<T, Eigen::Dynamic, 1> g(k + 1);
        g.setZero();
        g(0) = realValueToScalar<T>(beta);
        Eigen::Matrix<T, Eigen::Dynamic, 1> y
            = H.block(0, 0, k + 1, k).colPivHouseholderQr().solve(g);
        SparseAdvancedVector<T> candidate = x0 + V.block(0, 0, b.size(), k) * y;
        double residual = (b - A * candidate).norm();
        result.resvec.push_back(residual);
        result.relres = residual / normb;
        result.iter = static_cast<double>(k);
        result.x = candidate;
        if (result.relres <= tolerance) {
            result.flag = 0;
            return result;
        }
        if (hNext == 0) {
            result.flag = 4;
            return result;
        }
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
minresTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("minres");
    }

    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedVector<T> b = arrayToVector<T>(barg);
    double tolerance = optionalTolerance(argIn, 2, 1e-6);
    indexType maxIterations = optionalIterationCount(argIn, 3, std::min<indexType>(A.rows(), 20));
    ExplicitPreconditioner<T> M1;
    ExplicitPreconditioner<T> M2;
    if (argIn.size() > 4) {
        M1.compute(argIn[4], true, b.size(), eval);
    }
    if (argIn.size() > 5) {
        M2.compute(argIn[5], false, b.size(), eval);
    }
    SparseAdvancedVector<T> x0(b.size());
    x0.setZero();
    if (argIn.size() > 6 && !argIn[6].isEmpty()) {
        x0 = arrayToVector<T>(argIn[6]);
    }
    if (x0.size() != b.size()) {
        Error(L"Nelson:linear_algebra:initialGuessDimensionMismatch",
            L"Initial guess dimensions must agree.");
    }

    MinresResult<T> result = runMinres<T>(A, b, tolerance, maxIterations, M1, M2, x0);
    ArrayOfVector retval;
    retval << vectorToArrayOf<T>(dataClass, barg.dimensions(), result.x);
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(result.flag);
    }
    if (nLhs > 2) {
        retval << ArrayOf::doubleScalar(result.relres);
    }
    if (nLhs > 3) {
        retval << ArrayOf::doubleScalar(result.iter);
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
Nelson::LinearAlgebraGateway::minresBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 5);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("minres");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return minresTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return minresTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return minresTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return minresTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("minres");
    return {};
}
//=============================================================================
