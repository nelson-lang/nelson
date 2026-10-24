//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "odeBvpKinsolBuiltin.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <vector>
//=============================================================================
#if defined(NELSON_WITH_SUNDIALS) && defined(NELSON_WITH_SUNDIALS_KINSOL)
#include "odeSundialsHandles.hpp"
#include <kinsol/kinsol.h>
#include <kinsol/kinsol_ls.h>
#include <nvector/nvector_serial.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
std::vector<double>
realDoubleVector(const ArrayOf& value, const std::wstring& message)
{
    ArrayOf promoted = value;
    if (!promoted.isReal() || !promoted.canBePromotedTo(NLS_DOUBLE)) {
        Error(L"Nelson:ode:invalidArgument", message);
    }
    promoted.promoteType(NLS_DOUBLE);
    const auto* data = static_cast<const double*>(promoted.dataPointer());
    return std::vector<double>(data, data + promoted.elementCount());
}
//=============================================================================
ArrayOf
doubleColumn(const double* values, indexType n)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, n));
    std::copy(values, values + n, data);
    return ArrayOf(NLS_DOUBLE, Dimensions(n, 1), data);
}
//=============================================================================
ArrayOfVector
callFunction(Evaluator* eval, const ArrayOf& fcn, const ArrayOfVector& args, int nLhs)
{
    if (eval == nullptr || eval->getContext() == nullptr) {
        Error(L"Nelson:ode:evaluatorNotInitialized", L"Evaluator is not initialized.");
    }
    FunctionDef* funcDef = nullptr;
    if (!eval->getContext()->lookupFunction("feval", funcDef) || funcDef == nullptr) {
        Error(L"Nelson:ode:evaluatorNotAvailable", L"Function evaluator is not available.");
    }
    ArrayOfVector fevalArgs;
    fevalArgs << fcn;
    for (const auto& arg : args) {
        fevalArgs << arg;
    }
    return funcDef->evaluateFunction(eval, fevalArgs, nLhs);
}
//=============================================================================
ArrayOf
doubleVectorResult(const std::vector<double>& values)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, values.size()));
    std::copy(values.begin(), values.end(), data);
    return ArrayOf(NLS_DOUBLE, Dimensions(static_cast<indexType>(values.size()), 1), data);
}
//=============================================================================
#if defined(NELSON_WITH_SUNDIALS) && defined(NELSON_WITH_SUNDIALS_KINSOL)
//=============================================================================
using namespace Nelson::OdeSolversGateway::SundialsDetail;
//=============================================================================
struct KinsolProblem
{
    Evaluator* eval { nullptr };
    ArrayOf fcn;
    indexType n { 0 };
    std::wstring message;
};
//=============================================================================
void
storeException(KinsolProblem* problem, const NelsonException& e)
{
    problem->message = e.message();
}
//=============================================================================
int
kinsolResidual(N_Vector u, N_Vector f, void* userData)
{
    auto* problem = static_cast<KinsolProblem*>(userData);
    try {
        ArrayOfVector args;
        args << doubleColumn(NV_DATA_S(u), problem->n);
        ArrayOfVector out = callFunction(problem->eval, problem->fcn, args, 1);
        if (out.empty()) {
            Error(L"Nelson:bvp:residualNoValue", L"BVP residual function returned no value.");
        }
        std::vector<double> residual = realDoubleVector(out[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:bvpResidualFunctionReturn",
                L"BVP residual function must return doubles."));
        if (residual.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:bvp:residualSizeMismatch",
                L"BVP residual function returned a vector with incompatible size.");
        }
        std::copy(residual.begin(), residual.end(), NV_DATA_S(f));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
class KinsolMemoryHandle
{
public:
    explicit KinsolMemoryHandle(SUNContext sunctx)
    {
        value = KINCreate(sunctx);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:sundialsSolverAllocationFailed",
                L"Unable to allocate SUNDIALS memory.");
        }
    }

    ~KinsolMemoryHandle()
    {
        if (value != nullptr) {
            KINFree(&value);
        }
    }

    KinsolMemoryHandle(const KinsolMemoryHandle&) = delete;
    KinsolMemoryHandle&
    operator=(const KinsolMemoryHandle&)
        = delete;

    void*
    get() const
    {
        return value;
    }

private:
    void* value { nullptr };
};
//=============================================================================
ArrayOfVector
runKinsol(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isFunctionHandle()) {
        Error(L"Nelson:bvp:residualHandleExpected", L"BVP residual function handle expected.");
    }
    std::vector<double> initial = realDoubleVector(argIn[1],
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
            L"Initial vector"));
    const double tolerance = argIn[2].asDoubleScalar();
    KinsolProblem problem;
    problem.eval = eval;
    problem.fcn = argIn[0];
    problem.n = static_cast<indexType>(initial.size());

    SunContextHandle sunctx;
    SerialVectorHandle u(initial, sunctx.get());
    SerialVectorHandle scale(static_cast<sunindextype>(problem.n), sunctx.get());
    std::fill(scale.data(), scale.data() + problem.n, 1.0);
    DenseMatrixHandle matrix(problem.n, problem.n, sunctx.get());
    DenseLinearSolverHandle linearSolver(u.get(), matrix.get(), sunctx.get());
    KinsolMemoryHandle memory(sunctx.get());
    if (KINInit(memory.get(), kinsolResidual, u.get()) < 0
        || KINSetUserData(memory.get(), &problem) < 0
        || KINSetLinearSolver(memory.get(), linearSolver.get(), matrix.get()) < 0
        || KINSetFuncNormTol(memory.get(), tolerance) < 0
        || KINSetScaledStepTol(memory.get(), tolerance) < 0) {
        Error(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver.");
    }
    const int flag = KINSol(memory.get(), u.get(), KIN_LINESEARCH, scale.get(), scale.get());
    if (!problem.message.empty()) {
        RaiseError(problem.message, L"Nelson:ode:solveFailed");
    }
    if (flag < 0) {
        Error(L"Nelson:bvp:solveFailed", L"KINSOL failed to solve the BVP nonlinear system.");
    }
    long int iterations = 0;
    long int evaluations = 0;
    KINGetNumNonlinSolvIters(memory.get(), &iterations);
    KINGetNumFuncEvals(memory.get(), &evaluations);
    stringVector names { "niterations", "nfevals" };
    ArrayOfVector values;
    values << ArrayOf::doubleScalar(static_cast<double>(iterations));
    values << ArrayOf::doubleScalar(static_cast<double>(evaluations));
    std::vector<double> result(u.data(), u.data() + problem.n);
    ArrayOfVector retval;
    retval << doubleVectorResult(result);
    retval << ArrayOf::structArray(names, values);
    return retval;
}
#endif
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::OdeSolversGateway::odeBvpKinsolBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
#if defined(NELSON_WITH_SUNDIALS) && defined(NELSON_WITH_SUNDIALS_KINSOL)
    return runKinsol(eval, nLhs, argIn);
#else
    nargincheck(argIn, 3, 3);
    nargoutcheck(nLhs, 0, 2);
    Error(L"Nelson:bvp:backendNotAvailable", L"KINSOL backend is not available.");
    return {};
#endif
}
//=============================================================================
