//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "bicgBuiltin.hpp"
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
template <class T> struct BicgResult
{
    SparseAdvancedVector<T> x;
    double flag = 1;
    double relres = 0;
    double iter = 0;
    std::vector<double> resvec;
};
//=============================================================================
template <class T>
BicgResult<T>
runBicg(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    validateSystem<T>(A, b);
    BicgResult<T> result;
    result.x = x0;
    double normb = b.norm();
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> r = b - A * result.x;
    SparseAdvancedVector<T> rt = r;
    SparseAdvancedVector<T> p(r.size());
    SparseAdvancedVector<T> pt(r.size());
    p.setZero();
    pt.setZero();
    T rho = T(1);
    result.resvec.push_back(r.norm());
    result.relres = result.resvec.back() / normb;
    if (result.relres <= tolerance) {
        result.flag = 0;
        return result;
    }

    for (indexType k = 1; k <= maxIterations; ++k) {
        SparseAdvancedVector<T> z = applyPreconditioners<T>(M1, M2, r);
        SparseAdvancedVector<T> zt = applyPreconditionersAdjoint<T>(M1, M2, rt);
        T rhoNext = zt.dot(r);
        if (std::abs(rhoNext) == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        if (k == 1) {
            p = z;
            pt = zt;
        } else {
            T beta = rhoNext / rho;
            p = z + beta * p;
            pt = zt + Eigen::numext::conj(beta) * pt;
        }

        SparseAdvancedVector<T> q = A * p;
        SparseAdvancedVector<T> qt = A.adjoint() * pt;
        T denominator = pt.dot(q);
        if (std::abs(denominator) == 0) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }

        T alpha = rhoNext / denominator;
        result.x += alpha * p;
        r -= alpha * q;
        rt -= Eigen::numext::conj(alpha) * qt;

        result.resvec.push_back(r.norm());
        result.relres = result.resvec.back() / normb;
        result.iter = static_cast<double>(k);
        if (result.relres <= tolerance) {
            result.flag = 0;
            return result;
        }
        rho = rhoNext;
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
bicgTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("bicg");
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

    BicgResult<T> result = runBicg<T>(A, b, tolerance, maxIterations, M1, M2, x0);
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
Nelson::LinearAlgebraGateway::bicgBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 5);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("bicg");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return bicgTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return bicgTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return bicgTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return bicgTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("bicg");
    return {};
}
//=============================================================================
