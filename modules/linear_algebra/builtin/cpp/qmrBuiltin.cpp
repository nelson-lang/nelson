//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "qmrBuiltin.hpp"
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
template <class T> struct QmrResult
{
    SparseAdvancedVector<T> x;
    double flag = 1;
    double relres = 0;
    double iter = 0;
    std::vector<double> resvec;
};
//=============================================================================
template <class T>
bool
isZeroScalar(const T& value)
{
    return std::abs(value) == 0 || !std::isfinite(std::abs(value));
}
//=============================================================================
template <class T>
QmrResult<T>
runQmr(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    validateSystem<T>(A, b);
    QmrResult<T> result;
    result.x = x0;
    double normb = b.norm();
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> residual = b - A * result.x;
    result.resvec.push_back(residual.norm());
    result.relres = result.resvec.back() / normb;
    if (result.relres <= tolerance) {
        result.flag = 0;
        return result;
    }

    SparseAdvancedVector<T> vTilde = residual;
    SparseAdvancedVector<T> wTilde = residual;
    SparseAdvancedVector<T> y = M1.solve(vTilde);
    SparseAdvancedVector<T> z = M2.solveAdjoint(wTilde);
    RealScalar rho = y.norm();
    RealScalar xi = z.norm();
    if (rho == 0 || xi == 0 || !std::isfinite(rho) || !std::isfinite(xi)) {
        result.flag = 4;
        return result;
    }

    SparseAdvancedVector<T> v = vTilde / rho;
    y /= rho;
    SparseAdvancedVector<T> w = wTilde / xi;
    z /= xi;
    T delta = z.dot(y);
    SparseAdvancedVector<T> p(b.size());
    SparseAdvancedVector<T> q(b.size());
    SparseAdvancedVector<T> d(b.size());
    p.setZero();
    q.setZero();
    d.setZero();
    T epsilon = T(1);
    T eta = T(-1);
    RealScalar gamma = RealScalar(1);
    RealScalar theta = RealScalar(0);

    for (indexType k = 1; k <= maxIterations; ++k) {
        if (isZeroScalar<T>(delta)) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }

        SparseAdvancedVector<T> yTilde = M2.solve(y);
        SparseAdvancedVector<T> zTilde = M1.solveAdjoint(z);
        if (k == 1) {
            p = yTilde;
            q = zTilde;
        } else {
            p = yTilde - (xi * delta / epsilon) * p;
            q = zTilde - (rho * Eigen::numext::conj(delta) / Eigen::numext::conj(epsilon)) * q;
        }

        SparseAdvancedVector<T> pTilde = A * p;
        epsilon = q.dot(pTilde);
        if (isZeroScalar<T>(epsilon)) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        T beta = epsilon / delta;
        if (isZeroScalar<T>(beta)) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }

        vTilde = pTilde - beta * v;
        y = M1.solve(vTilde);
        RealScalar rhoNext = y.norm();
        wTilde = A.adjoint() * q - Eigen::numext::conj(beta) * w;
        z = M2.solveAdjoint(wTilde);
        RealScalar xiNext = z.norm();

        RealScalar thetaNext = rhoNext / (gamma * static_cast<RealScalar>(std::abs(beta)));
        RealScalar gammaNext = RealScalar(1) / std::sqrt(RealScalar(1) + thetaNext * thetaNext);
        if (!std::isfinite(thetaNext) || !std::isfinite(gammaNext)) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }

        T etaNext = -eta * rho * (gammaNext * gammaNext) / (beta * gamma * gamma);
        d = etaNext * p + (theta * gammaNext) * (theta * gammaNext) * d;
        result.x += d;

        residual = b - A * result.x;
        result.resvec.push_back(residual.norm());
        result.relres = result.resvec.back() / normb;
        result.iter = static_cast<double>(k);
        if (result.relres <= tolerance) {
            result.flag = 0;
            return result;
        }

        if (rhoNext == 0 || xiNext == 0 || !std::isfinite(rhoNext) || !std::isfinite(xiNext)) {
            result.flag = 4;
            return result;
        }
        v = vTilde / rhoNext;
        y /= rhoNext;
        w = wTilde / xiNext;
        z /= xiNext;
        rho = rhoNext;
        xi = xiNext;
        gamma = gammaNext;
        theta = thetaNext;
        eta = etaNext;
        delta = z.dot(y);
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
qmrTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("qmr");
    }
    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedVector<T> b = arrayToVector<T>(barg);
    double tolerance = optionalTolerance(argIn, 2, 1e-6);
    indexType defaultMax = std::min<indexType>(A.rows(), 20);
    indexType maxIterations = optionalIterationCount(argIn, 3, defaultMax);
    ExplicitPreconditioner<T> M1;
    ExplicitPreconditioner<T> M2;
    if (argIn.size() > 4) {
        M1.compute(argIn[4], true, b.size(), eval, true);
    }
    if (argIn.size() > 5) {
        M2.compute(argIn[5], false, b.size(), eval, true);
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

    QmrResult<T> result = runQmr<T>(A, b, tolerance, maxIterations, M1, M2, x0);
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
Nelson::LinearAlgebraGateway::qmrBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 5);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("qmr");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return qmrTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return qmrTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return qmrTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return qmrTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("qmr");
    return {};
}
//=============================================================================
