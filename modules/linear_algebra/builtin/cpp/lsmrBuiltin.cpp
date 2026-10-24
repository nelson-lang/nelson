//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lsmrBuiltin.hpp"
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
template <class T> struct LsmrResult
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
LsmrResult<T>
runLsmr(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;

    if (A.rows() != b.size()) {
        Error(L"Nelson:linear_algebra:dimensionMismatch",
            L"Matrix and vector dimensions must agree.");
    }

    LsmrResult<T> result;
    result.x = x0;
    double normb = b.norm();
    const bool zeroRightHandSide = normb == 0;
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> y(A.cols());
    y.setZero();
    SparseAdvancedVector<T> residual = b - A * result.x;
    double beta = residual.norm();
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
    if (maxIterations == 0) {
        return result;
    }

    SparseAdvancedVector<T> u = residual / static_cast<RealScalar>(beta);
    SparseAdvancedVector<T> v = applyRightPreconditionerAdjoint<T>(M1, M2, A.adjoint() * u);
    double alpha = v.norm();
    result.lsvec.push_back(alpha * beta);
    if (alpha == 0) {
        result.flag = 0;
        return result;
    }
    v /= static_cast<RealScalar>(alpha);

    double alphabar = alpha;
    double zetabar = alpha * beta;
    double rhoPrevious = 1;
    double rhobarPrevious = 1;
    double cbarPrevious = 1;
    double sbarPrevious = 0;
    double betaDotted = 0;
    double betaDoubledot = beta;
    double rhoDottedPrevious = 1;
    double thetaTildePrevious = 0;
    double tauTildePrevious = 0;
    double zetaPrevious = 0;
    SparseAdvancedVector<T> h = v;
    SparseAdvancedVector<T> hbarPrevious(A.cols());
    hbarPrevious.setZero();
    double initialNormalResidual = result.lsvec.front();
    if (initialNormalResidual == 0) {
        initialNormalResidual = 1;
    }

    for (indexType k = 1; k <= maxIterations; ++k) {
        u = A * applyRightPreconditioner<T>(M1, M2, v) - static_cast<RealScalar>(alpha) * u;
        beta = u.norm();
        if (beta != 0) {
            u /= static_cast<RealScalar>(beta);
        }

        SparseAdvancedVector<T> vNext = applyRightPreconditionerAdjoint<T>(M1, M2, A.adjoint() * u)
            - static_cast<RealScalar>(beta) * v;
        double alphaNext = vNext.norm();
        if (alphaNext != 0) {
            vNext /= static_cast<RealScalar>(alphaNext);
        }

        double rho = std::hypot(alphabar, beta);
        if (rho == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        double c = alphabar / rho;
        double s = beta / rho;
        double theta = s * alphaNext;
        alphabar = c * alphaNext;

        double thetabar = sbarPrevious * rho;
        double rhobar = std::hypot(cbarPrevious * rho, theta);
        if (rhobar == 0 || rhoPrevious == 0 || rhobarPrevious == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        double cbar = cbarPrevious * rho / rhobar;
        double sbar = theta / rhobar;
        double zeta = cbar * zetabar;
        zetabar = -sbar * zetabar;

        SparseAdvancedVector<T> hbar = h
            - static_cast<RealScalar>(thetabar * rho / (rhoPrevious * rhobarPrevious))
                * hbarPrevious;
        y += static_cast<RealScalar>(zeta / (rho * rhobar)) * hbar;
        h = vNext - static_cast<RealScalar>(theta / rho) * h;
        result.x = x0 + applyRightPreconditioner<T>(M1, M2, y);

        double betaHat = c * betaDoubledot;
        betaDoubledot = -s * betaDoubledot;
        double rhoTilde = std::hypot(rhoDottedPrevious, thetabar);
        if (rhoTilde == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        double cTilde = rhoDottedPrevious / rhoTilde;
        double sTilde = thetabar / rhoTilde;
        double thetaTilde = sTilde * rhobar;
        double rhoDotted = cTilde * rhobar;
        if (rhoDotted == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        betaDotted = -sTilde * betaDotted + cTilde * betaHat;
        double tauTilde = (zetaPrevious - thetaTildePrevious * tauTildePrevious) / rhoTilde;
        double tauDotted = (zeta - thetaTilde * tauTilde) / rhoDotted;

        result.iter = static_cast<double>(k);
        result.resvec.push_back(
            std::sqrt(std::pow(betaDotted - tauDotted, 2) + std::pow(betaDoubledot, 2)));
        result.lsvec.push_back(std::abs(zetabar));
        result.relres = result.resvec.back() / normb;
        if (result.relres <= tolerance
            || result.lsvec.back() <= tolerance * initialNormalResidual) {
            result.flag = 0;
            return result;
        }
        if (alphaNext == 0 || beta == 0) {
            result.flag = 0;
            return result;
        }

        hbarPrevious = hbar;
        rhoPrevious = rho;
        rhobarPrevious = rhobar;
        cbarPrevious = cbar;
        sbarPrevious = sbar;
        rhoDottedPrevious = rhoDotted;
        thetaTildePrevious = thetaTilde;
        tauTildePrevious = tauTilde;
        zetaPrevious = zeta;
        v = vNext;
        alpha = alphaNext;
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
lsmrTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("lsmr");
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

    LsmrResult<T> result = runLsmr<T>(A, b, tolerance, maxIterations, M1, M2, x0);
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
Nelson::LinearAlgebraGateway::lsmrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 6);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("lsmr");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return lsmrTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return lsmrTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return lsmrTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return lsmrTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("lsmr");
    return {};
}
//=============================================================================
