//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lsqrBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "SparseAdvancedHelpers.hpp"
#include "ErrorID.hpp"
#include <limits>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
template <class T> struct LsqrResult
{
    SparseAdvancedVector<T> x;
    double flag = 1;
    double relres = 0;
    double iter = 0;
    std::vector<double> resvec;
    std::vector<double> lsvec;
};
//=============================================================================
template <class T>
SparseAdvancedVector<T>
applyRightPreconditioner(const ExplicitPreconditioner<T>& M1, const ExplicitPreconditioner<T>& M2,
    const SparseAdvancedVector<T>& rhs)
{
    return applyPreconditioners<T>(M1, M2, rhs);
}
//=============================================================================
template <class T>
SparseAdvancedVector<T>
applyRightPreconditionerAdjoint(const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& rhs)
{
    return applyPreconditionersAdjoint<T>(M1, M2, rhs);
}
//=============================================================================
template <class T>
LsqrResult<T>
runLsqr(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;

    if (A.rows() != b.size()) {
        Error(L"Nelson:linear_algebra:dimensionMismatch",
            L"Matrix and vector dimensions must agree.");
    }

    LsqrResult<T> result;
    result.x = x0;
    double normb = b.norm();
    const bool zeroRightHandSide = normb == 0;
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> y(A.cols());
    y.setZero();
    SparseAdvancedVector<T> u = b - A * result.x;
    double beta = u.norm();
    result.resvec.push_back(beta);
    result.relres = beta / normb;
    if (result.relres <= tolerance) {
        result.flag = 0;
        if (zeroRightHandSide && beta == 0) {
            result.relres = std::numeric_limits<double>::quiet_NaN();
            return result;
        }
        result.lsvec.push_back(0);
        return result;
    }
    u /= static_cast<RealScalar>(beta);

    SparseAdvancedVector<T> v = applyRightPreconditionerAdjoint<T>(M1, M2, A.adjoint() * u);
    double alpha = v.norm();
    if (alpha == 0) {
        result.flag = 4;
        result.lsvec.push_back(0);
        return result;
    }
    v /= static_cast<RealScalar>(alpha);
    SparseAdvancedVector<T> w = v;
    double phibar = beta;
    double rhobar = alpha;
    double normAEstimate = 0;
    double normar = alpha * beta;

    for (indexType k = 1; k <= maxIterations; ++k) {
        double previousAlpha = alpha;
        SparseAdvancedVector<T> Av = A * applyRightPreconditioner<T>(M1, M2, v);
        u = Av - static_cast<RealScalar>(alpha) * u;
        beta = u.norm();
        if (beta != 0) {
            u /= static_cast<RealScalar>(beta);
        }
        normAEstimate = std::hypot(std::hypot(normAEstimate, previousAlpha), beta);
        if (normAEstimate != 0) {
            result.lsvec.push_back(normar / normAEstimate);
        } else {
            result.lsvec.push_back(0);
        }

        SparseAdvancedVector<T> Atu = applyRightPreconditionerAdjoint<T>(M1, M2, A.adjoint() * u);
        v = Atu - static_cast<RealScalar>(beta) * v;
        alpha = v.norm();
        if (alpha != 0) {
            v /= static_cast<RealScalar>(alpha);
        }

        double rho = std::hypot(rhobar, beta);
        if (rho == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        double c = rhobar / rho;
        double s = beta / rho;
        double theta = s * alpha;
        rhobar = -c * alpha;
        double phi = c * phibar;
        phibar = s * phibar;

        y += static_cast<RealScalar>(phi / rho) * w;
        w = v - static_cast<RealScalar>(theta / rho) * w;
        result.x = x0 + applyRightPreconditioner<T>(M1, M2, y);
        result.resvec.push_back(std::abs(phibar));
        normar = alpha * std::abs(s * phi);
        double leastSquaresResidual = normAEstimate != 0 ? normar / normAEstimate : 0;
        result.relres = result.resvec.back() / normb;
        result.iter = static_cast<double>(k);
        double leastSquaresCriterion = tolerance;
        if (!result.lsvec.empty()) {
            leastSquaresCriterion *= result.lsvec.front();
        }
        if (result.relres <= tolerance || leastSquaresResidual <= leastSquaresCriterion) {
            result.lsvec.push_back(leastSquaresResidual);
            result.flag = 0;
            return result;
        }
        if (alpha == 0 || beta == 0) {
            result.flag = 0;
            return result;
        }
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
lsqrTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("lsqr");
    }
    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedVector<T> b = arrayToVector<T>(barg);
    double tolerance = optionalTolerance(argIn, 2, 1e-6);
    indexType maxIterations = optionalIterationCount(argIn, 3, std::min<indexType>(A.cols(), 20));
    ExplicitPreconditioner<T> M1;
    ExplicitPreconditioner<T> M2;
    if (argIn.size() > 4) {
        M1.compute(argIn[4], true, A.cols(), eval, true);
    }
    if (argIn.size() > 5) {
        M2.compute(argIn[5], false, A.cols(), eval, true);
    }
    SparseAdvancedVector<T> x0(A.cols());
    x0.setZero();
    if (argIn.size() > 6 && !argIn[6].isEmpty()) {
        x0 = arrayToVector<T>(argIn[6]);
    }
    if (x0.size() != A.cols()) {
        Error(L"Nelson:linear_algebra:initialGuessDimensionMismatch",
            L"Initial guess dimensions must agree.");
    }

    LsqrResult<T> result = runLsqr<T>(A, b, tolerance, maxIterations, M1, M2, x0);
    ArrayOfVector retval;
    retval << vectorToArrayOf<T>(dataClass, Dimensions(Aarg.columns(), 1), result.x);
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
    if (nLhs > 5) {
        retval << doubleColumnVector(result.lsvec);
    }
    return retval;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::lsqrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 6);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("lsqr");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return lsqrTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return lsqrTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return lsqrTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return lsqrTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("lsqr");
    return {};
}
//=============================================================================
