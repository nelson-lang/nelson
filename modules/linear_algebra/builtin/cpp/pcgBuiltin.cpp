//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pcgBuiltin.hpp"
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
template <class T> struct IterativeResult
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
isBreakdownScalar(const T& value)
{
    double magnitude = std::abs(value);
    return !std::isfinite(magnitude) || magnitude == 0;
}
//=============================================================================
template <class T>
IterativeResult<T>
runPcg(const SparseAdvancedMatrix<T>& A, const SparseAdvancedVector<T>& b, double tolerance,
    indexType maxIterations, const ExplicitPreconditioner<T>& M1,
    const ExplicitPreconditioner<T>& M2, const SparseAdvancedVector<T>& x0)
{
    validateSystem<T>(A, b);
    IterativeResult<T> result;
    result.x = x0;
    double normb = b.norm();
    if (normb == 0) {
        normb = 1;
    }

    SparseAdvancedVector<T> r = b - A * result.x;
    result.resvec.push_back(r.norm());
    result.relres = result.resvec.back() / normb;
    if (result.relres <= tolerance) {
        result.flag = 0;
        return result;
    }

    SparseAdvancedVector<T> z = applyPreconditioners<T>(M1, M2, r);
    SparseAdvancedVector<T> p = z;
    T rho = r.dot(z);
    if (isBreakdownScalar<T>(rho)) {
        result.flag = 4;
        return result;
    }

    for (indexType k = 1; k <= maxIterations; ++k) {
        SparseAdvancedVector<T> q = A * p;
        T denominator = p.dot(q);
        if (isBreakdownScalar<T>(denominator)) {
            result.flag = 4;
            result.iter = static_cast<double>(k - 1);
            return result;
        }
        T alpha = rho / denominator;
        result.x += alpha * p;
        r -= alpha * q;
        result.resvec.push_back(r.norm());
        result.relres = result.resvec.back() / normb;
        result.iter = static_cast<double>(k);
        if (result.relres <= tolerance) {
            result.flag = 0;
            return result;
        }
        z = applyPreconditioners<T>(M1, M2, r);
        T rhoNext = r.dot(z);
        if (isBreakdownScalar<T>(rhoNext)) {
            result.flag = 4;
            return result;
        }
        p = z + (rhoNext / rho) * p;
        rho = rhoNext;
    }
    return result;
}
//=============================================================================
template <class T>
ArrayOfVector
pcgTyped(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, NelsonType dataClass)
{
    ArrayOf Aarg = argIn[0];
    ArrayOf barg = argIn[1];
    if (!Aarg.isSparse() || !isCompatibleFloatingClass(Aarg, dataClass)
        || !isCompatibleFloatingClass(barg, dataClass)) {
        OverloadRequired("pcg");
    }

    SparseAdvancedMatrix<T> A = arrayToSparseMatrix<T>(Aarg);
    SparseAdvancedVector<T> b = arrayToVector<T>(barg);
    double tolerance = optionalTolerance(argIn, 2, 1e-6);
    indexType defaultMax = std::min<indexType>(A.rows(), 20);
    indexType maxIterations = optionalIterationCount(argIn, 3, defaultMax);
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

    IterativeResult<T> result = runPcg<T>(A, b, tolerance, maxIterations, M1, M2, x0);
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
Nelson::LinearAlgebraGateway::pcgBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 7);
    nargoutcheck(nLhs, 0, 5);
    if (!argIn[0].isSparse() || !isFloatingPointClass(argIn[0].dataClass())
        || !isFloatingPointClass(argIn[1].dataClass())) {
        OverloadRequired("pcg");
    }
    if (argIn[0].dataClass() == NLS_SCOMPLEX || argIn[1].dataClass() == NLS_SCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_SCOMPLEX)) {
        return pcgTyped<singlecomplex>(eval, nLhs, argIn, NLS_SCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_DCOMPLEX || argIn[1].dataClass() == NLS_DCOMPLEX
        || anyArgumentHasDataClass(argIn, { 4, 5, 6 }, NLS_DCOMPLEX)) {
        return pcgTyped<doublecomplex>(eval, nLhs, argIn, NLS_DCOMPLEX);
    }
    if (argIn[0].dataClass() == NLS_SINGLE) {
        return pcgTyped<single>(eval, nLhs, argIn, NLS_SINGLE);
    }
    if (argIn[0].dataClass() == NLS_DOUBLE) {
        return pcgTyped<double>(eval, nLhs, argIn, NLS_DOUBLE);
    }
    OverloadRequired("pcg");
    return {};
}
//=============================================================================
