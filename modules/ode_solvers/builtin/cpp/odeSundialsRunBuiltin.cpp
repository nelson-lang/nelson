//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "odeSundialsRunBuiltin.hpp"
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "FunctionDef.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "odeSundialsHandles.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <limits>
#include <memory>
#include <string>
#include <vector>
//=============================================================================
#ifdef NELSON_WITH_SUNDIALS
#include <cvodes/cvodes.h>
#include <cvodes/cvodes_ls.h>
#include <idas/idas.h>
#include <idas/idas_ls.h>
#include <nvector/nvector_serial.h>
#include <sundials/sundials_context.h>
#include <sundials/sundials_linearsolver.h>
#include <sundials/sundials_matrix.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <Eigen/Sparse>
#endif
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
constexpr const char* FIELD_SOLVER = "solver";
constexpr const char* FIELD_ODEFUN = "odefun";
constexpr const char* FIELD_TSPAN = "tspan";
constexpr const char* FIELD_Y0 = "y0";
constexpr const char* FIELD_YP0 = "yp0";
constexpr const char* FIELD_OPTIONS = "options";
constexpr const char* FIELD_DIRECTION = "direction";
constexpr const char* FIELD_SENSITIVITY = "Sensitivity";
//=============================================================================
ArrayOf
getOptionalField(const ArrayOf& value, const std::string& name)
{
    if (value.isField(name)) {
        return value.field(name);
    }
    return ArrayOf::empty();
}
//=============================================================================
bool
isOn(const ArrayOf& value)
{
    if (value.isEmpty()) {
        return false;
    }
    if (value.isCharacterArray() || value.isStringArray()) {
        return value.asWideString() == L"on";
    }
    return value.asDoubleScalar() != 0;
}
//=============================================================================
std::vector<double>
realDoubleVector(const ArrayOf& value, const std::wstring& message)
{
    if (value.dataClass() != NLS_DOUBLE || !value.isReal()) {
        Error(L"Nelson:ode:invalidArgument", message);
    }
    const auto* data = static_cast<const double*>(value.dataPointer());
    return std::vector<double>(data, data + value.elementCount());
}
//=============================================================================
std::vector<double>
realNumericVector(const ArrayOf& value, const std::wstring& message, bool allowLogical)
{
    if (allowLogical && value.dataClass() == NLS_LOGICAL && value.isReal()) {
        const auto* data = static_cast<const logical*>(value.dataPointer());
        std::vector<double> result(value.elementCount());
        for (indexType k = 0; k < value.elementCount(); ++k) {
            result[k] = data[k] != 0;
        }
        return result;
    }
    ArrayOf promoted = value;
    if (!promoted.isReal() || !promoted.canBePromotedTo(NLS_DOUBLE)) {
        Error(L"Nelson:ode:invalidArgument", message);
    }
    promoted.promoteType(NLS_DOUBLE);
    return realDoubleVector(promoted, message);
}
//=============================================================================
ArrayOf
doubleArray(indexType rows, indexType cols, const std::vector<double>& values)
{
    double* data = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, rows * cols));
    std::copy(values.begin(), values.end(), data);
    return ArrayOf(NLS_DOUBLE, Dimensions(rows, cols), data);
}
//=============================================================================
ArrayOf
doubleTensor(indexType rows, indexType cols, indexType pages, const std::vector<double>& values)
{
    double* data = static_cast<double*>(
        ArrayOf::allocateArrayOf(NLS_DOUBLE, static_cast<size_t>(rows * cols * pages)));
    std::copy(values.begin(), values.end(), data);
    return ArrayOf(NLS_DOUBLE, Dimensions(std::vector<indexType> { rows, cols, pages }), data);
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
std::vector<int>
integerVector(const ArrayOf& value, const std::wstring& message)
{
    const std::vector<double> values = realDoubleVector(value, message);
    std::vector<int> result(values.size());
    for (size_t k = 0; k < values.size(); ++k) {
        result[k] = static_cast<int>(values[k]);
    }
    return result;
}
//=============================================================================
ArrayOfVector
cellValues(const ArrayOf& value)
{
    ArrayOfVector result;
    if (value.isEmpty()) {
        return result;
    }
    if (!value.isCell()) {
        result << value;
        return result;
    }
    const auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
    const indexType count = value.elementCount();
    for (indexType k = 0; k < count; ++k) {
        result << elements[k];
    }
    return result;
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
ArrayOfVector
callProblemFunction(Evaluator* eval, const ArrayOf& fcn, double t, const double* y, indexType n,
    const ArrayOfVector& parameters, int nLhs)
{
    ArrayOfVector args;
    args << ArrayOf::doubleScalar(t);
    args << doubleColumn(y, n);
    for (const auto& parameter : parameters) {
        args << parameter;
    }
    return callFunction(eval, fcn, args, nLhs);
}
//=============================================================================
ArrayOfVector
callImplicitFunction(Evaluator* eval, const ArrayOf& fcn, double t, const double* y,
    const double* yp, indexType n, const ArrayOfVector& parameters, int nLhs)
{
    ArrayOfVector args;
    args << ArrayOf::doubleScalar(t);
    args << doubleColumn(y, n);
    args << doubleColumn(yp, n);
    for (const auto& parameter : parameters) {
        args << parameter;
    }
    return callFunction(eval, fcn, args, nLhs);
}
//=============================================================================
std::vector<double>
solveDense(std::vector<double> matrix, std::vector<double> rhs)
{
    const indexType n = static_cast<indexType>(rhs.size());
    for (indexType k = 0; k < n; ++k) {
        indexType pivot = k;
        double pivotAbs = std::abs(matrix[k + k * n]);
        for (indexType row = k + 1; row < n; ++row) {
            const double candidate = std::abs(matrix[row + k * n]);
            if (candidate > pivotAbs) {
                pivotAbs = candidate;
                pivot = row;
            }
        }
        if (pivotAbs == 0) {
            Error(L"Nelson:ode_solvers:massMatrixSingular", L"Mass matrix is singular.");
        }
        if (pivot != k) {
            for (indexType col = 0; col < n; ++col) {
                std::swap(matrix[k + col * n], matrix[pivot + col * n]);
            }
            std::swap(rhs[k], rhs[pivot]);
        }
        for (indexType row = k + 1; row < n; ++row) {
            const double factor = matrix[row + k * n] / matrix[k + k * n];
            matrix[row + k * n] = 0;
            for (indexType col = k + 1; col < n; ++col) {
                matrix[row + col * n] -= factor * matrix[k + col * n];
            }
            rhs[row] -= factor * rhs[k];
        }
    }
    std::vector<double> x(n, 0);
    for (indexType row = n; row-- > 0;) {
        double value = rhs[row];
        for (indexType col = row + 1; col < n; ++col) {
            value -= matrix[row + col * n] * x[col];
        }
        x[row] = value / matrix[row + row * n];
    }
    return x;
}
//=============================================================================
struct EventData
{
    int count { 0 };
    std::vector<int> terminal;
    std::vector<int> direction;
};
//=============================================================================
struct SparseIlu0Factor
{
    indexType n { 0 };
    std::vector<indexType> rowStart;
    std::vector<indexType> columns;
    std::vector<indexType> columnStart;
    std::vector<indexType> rows;
    std::vector<double> values;
};
//=============================================================================
struct NativeProblem
{
    Evaluator* eval { nullptr };
    std::string solver;
    ArrayOf odefun;
    ArrayOf options;
    ArrayOf mass;
    ArrayOf events;
    ArrayOf eventDefinitionObject;
    ArrayOfVector parameters;
    EventData eventData;
    indexType n { 0 };
    ArrayOf yp0;
    bool useNativeSensitivity { false };
    bool useAdjointSensitivity { false };
    indexType sensitivityCount { 0 };
    std::vector<double> sensitivityParameters;
    std::vector<double> sensitivityScales;
    std::vector<int> sensitivityParameterList;
    std::vector<double> sensitivityInitialValue;
    std::vector<double> sensitivityInitialSlope;
    ArrayOf adjointObjective;
    ArrayOf adjointQuadrature;
    double adjointObjectiveTime { std::numeric_limits<double>::quiet_NaN() };
    double adjointRelativeTolerance { std::numeric_limits<double>::quiet_NaN() };
    double adjointAbsoluteTolerance { std::numeric_limits<double>::quiet_NaN() };
    std::vector<double> preconditionerDiagonal;
    std::vector<double> preconditionerMatrix;
    SparseIlu0Factor preconditionerIlu0;
    long int preconditionerSetupCount { 0 };
    long int preconditionerSolveCount { 0 };
    std::wstring callbackError;
};
//=============================================================================
void
storeException(NativeProblem* problem, const NelsonException& e)
{
    problem->callbackError = e.message();
}
//=============================================================================
ArrayOfVector
currentParameters(NativeProblem* problem)
{
    if (!problem->useNativeSensitivity && !problem->useAdjointSensitivity) {
        return problem->parameters;
    }
    ArrayOfVector values;
    values << doubleColumn(problem->sensitivityParameters.data(),
        static_cast<indexType>(problem->sensitivityParameters.size()));
    return values;
}
//=============================================================================
#ifdef NELSON_WITH_SUNDIALS
//=============================================================================
using namespace Nelson::OdeSolversGateway::SundialsDetail;
//=============================================================================
#include "odeSundialsCallbacks.hpp"
//=============================================================================
#include "odeSundialsSparse.hpp"
#include "odeSundialsPreconditioner.hpp"
#include "odeSundialsSolutionHelpers.hpp"
//=============================================================================
void
setCvodesTolerances(void* memory, const NativeProblem& problem, SUNContext sunctx)
{
    const double relTol = getOptionalField(problem.options, "RelTol").asDoubleScalar();
    const std::vector<double> absTol = realDoubleVector(getOptionalField(problem.options, "AbsTol"),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
            L"Absolute tolerance"));
    if (absTol.size() != static_cast<size_t>(problem.n)) {
        Error(L"Nelson:ode_solvers:absoluteToleranceSizeNotMatchInitial",
            L"Absolute tolerance size does not match initial value.");
    }
    SerialVectorHandle absTolVector(absTol, sunctx);
    checkFlag(CVodeSVtolerances(memory, relTol, absTolVector.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
}
//=============================================================================
void
setIdasTolerances(void* memory, const NativeProblem& problem, SUNContext sunctx)
{
    const double relTol = getOptionalField(problem.options, "RelTol").asDoubleScalar();
    const std::vector<double> absTol = realDoubleVector(getOptionalField(problem.options, "AbsTol"),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
            L"Absolute tolerance"));
    if (absTol.size() != static_cast<size_t>(problem.n)) {
        Error(L"Nelson:ode_solvers:absoluteToleranceSizeNotMatchInitial",
            L"Absolute tolerance size does not match initial value.");
    }
    SerialVectorHandle absTolVector(absTol, sunctx);
    checkFlag(IDASVtolerances(memory, relTol, absTolVector.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
}
//=============================================================================
std::unique_ptr<SerialVectorHandle>
nonNegativeConstraints(const NativeProblem& problem, SUNContext sunctx)
{
    ArrayOf nonNegative = getOptionalField(problem.options, "NonNegative");
    if (nonNegative.isEmpty()) {
        return nullptr;
    }
    std::vector<double> values = realDoubleVector(nonNegative,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:nonnegativeIndicesInvalid", L"NonNegative indices are invalid."));
    auto constraints = std::make_unique<SerialVectorHandle>(problem.n, sunctx);
    std::fill(constraints->data(), constraints->data() + problem.n, 0.0);
    for (double value : values) {
        if (!std::isfinite(value) || value < 1 || value > static_cast<double>(problem.n)
            || std::floor(value) != value) {
            Error(L"Nelson:ode_solvers:nonnegativeIndicesInvalid",
                L"NonNegative indices are invalid.");
        }
        constraints->data()[static_cast<indexType>(value) - 1] = 1.0;
    }
    return constraints;
}
//=============================================================================
SundialsLinearStats
cvodesLinearStats(void* memory, const NativeProblem& problem)
{
    SundialsLinearStats stats;
    CVodeGetNumLinSolvSetups(memory, &stats.linearSetupCount);
    CVodeGetNumLinIters(memory, &stats.linearIterations);
    CVodeGetNumLinConvFails(memory, &stats.linearConvergenceFailures);
    CVodeGetNumPrecEvals(memory, &stats.preconditionerSetupCount);
    CVodeGetNumPrecSolves(memory, &stats.preconditionerSolveCount);
    if (stats.preconditionerSetupCount == 0) {
        stats.preconditionerSetupCount = problem.preconditionerSetupCount;
    }
    if (stats.preconditionerSolveCount == 0) {
        stats.preconditionerSolveCount = problem.preconditionerSolveCount;
    }
    return stats;
}
//=============================================================================
SundialsLinearStats
idasLinearStats(void* memory, const NativeProblem& problem)
{
    SundialsLinearStats stats;
    IDAGetNumLinSolvSetups(memory, &stats.linearSetupCount);
    IDAGetNumLinIters(memory, &stats.linearIterations);
    IDAGetNumLinConvFails(memory, &stats.linearConvergenceFailures);
    IDAGetNumPrecEvals(memory, &stats.preconditionerSetupCount);
    IDAGetNumPrecSolves(memory, &stats.preconditionerSolveCount);
    if (stats.preconditionerSetupCount == 0) {
        stats.preconditionerSetupCount = problem.preconditionerSetupCount;
    }
    if (stats.preconditionerSolveCount == 0) {
        stats.preconditionerSolveCount = problem.preconditionerSolveCount;
    }
    return stats;
}
//=============================================================================
#include "odeSundialsAdjoint.hpp"
//=============================================================================
struct LinearSolverHandles
{
    std::unique_ptr<DenseMatrixHandle> matrix;
    std::unique_ptr<DenseLinearSolverHandle> denseLinearSolver;
    std::unique_ptr<IterativeLinearSolverHandle> iterativeLinearSolver;
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
    std::unique_ptr<SparseMatrixHandle> sparseMatrix;
    std::unique_ptr<SparseDirectLinearSolverHandle> sparseLinearSolver;
#endif
};
//=============================================================================
// Shared linear solver selection for the CVODES and IDAS drivers. The three
// callables perform the solver-family specific attachments: the linear
// solver itself, the mandatory sparse Jacobian routine of the sparse direct
// path, and the preconditioner pair (keyed by preconditioner name).
template <typename SetLinearSolver, typename SetSparseJacobian, typename SetPreconditioner>
void
attachLinearSolver(NativeProblem& problem, N_Vector y, SUNContext sunctx,
    const std::string& linearSolverName, const std::string& preconditionerName,
    LinearSolverHandles& handles, SetLinearSolver setLinearSolver,
    SetSparseJacobian setSparseJacobian, SetPreconditioner setPreconditioner)
{
    if (useSparseDirectLinearSolver(linearSolverName)) {
        if (preconditionerName != "none") {
            Error(L"Nelson:ode:unsupportedPreconditioner", L"Unsupported SUNDIALS preconditioner.");
        }
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
        const sunindextype dimension = static_cast<sunindextype>(problem.n);
        const sunindextype nonzeros = std::max<sunindextype>(1, dimension * dimension);
        handles.sparseMatrix
            = std::make_unique<SparseMatrixHandle>(problem.n, problem.n, nonzeros, sunctx);
        handles.sparseLinearSolver = std::make_unique<SparseDirectLinearSolverHandle>(
            y, handles.sparseMatrix->get(), sunctx);
        setLinearSolver(handles.sparseLinearSolver->get(), handles.sparseMatrix->get());
        // A sparse matrix has no difference-quotient fallback in SUNDIALS: a
        // Jacobian routine is mandatory.
        setSparseJacobian();
#else
        Error(L"Nelson:ode:kluNotAvailable", L"SUNDIALS KLU linear solver is not available.");
#endif
        return;
    }
    if (useIterativeLinearSolver(linearSolverName)) {
        const bool usePreconditioner = preconditionerName == "jacobi"
            || preconditionerName == "banded" || preconditionerName == "ilu0";
        if (!usePreconditioner && preconditionerName != "none") {
            Error(L"Nelson:ode:unsupportedPreconditioner", L"Unsupported SUNDIALS preconditioner.");
        }
        handles.iterativeLinearSolver = std::make_unique<IterativeLinearSolverHandle>(
            y, linearSolverName, usePreconditioner, sunctx);
        setLinearSolver(handles.iterativeLinearSolver->get(), nullptr);
        if (usePreconditioner) {
            setPreconditioner(preconditionerName);
        }
        return;
    }
    handles.matrix = std::make_unique<DenseMatrixHandle>(problem.n, problem.n, sunctx);
    handles.denseLinearSolver
        = std::make_unique<DenseLinearSolverHandle>(y, handles.matrix->get(), sunctx);
    setLinearSolver(handles.denseLinearSolver->get(), handles.matrix->get());
}
//=============================================================================
ArrayOf
runCvodes(NativeProblem& problem, const std::vector<double>& tspan, const std::vector<double>& y0)
{
    if (problem.useAdjointSensitivity) {
        return runCvodesAdjoint(problem, tspan, y0);
    }
    const std::vector<double> outputTimes = solverOutputTimes(problem, tspan);
    SunContextHandle sunctx;
    SerialVectorHandle y(problem.n, sunctx.get());
    SerialVectorHandle yd(problem.n, sunctx.get());
    CvodesMemoryHandle memory(problem.solver == "cvodesnonstiff" ? CV_ADAMS : CV_BDF, sunctx.get());
    std::unique_ptr<SerialVectorArrayHandle> sensitivityVectors;
    std::unique_ptr<SerialVectorHandle> constraints;
    std::string linearSolverName = linearSolverNameForProblem(problem);
    std::string preconditionerName = preconditionerNameForProblem(problem, linearSolverName);
    LinearSolverHandles linearSolverHandles;
    std::copy(y0.begin(), y0.end(), y.data());
    checkFlag(CVodeInit(memory.get(), cvodesRhs, tspan.front(), y.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    if (problem.useNativeSensitivity) {
        sensitivityVectors = std::make_unique<SerialVectorArrayHandle>(
            problem.sensitivityCount, problem.n, problem.sensitivityInitialValue, sunctx.get());
        checkFlag(CVodeSensInit(memory.get(), static_cast<int>(problem.sensitivityCount),
                      CV_SIMULTANEOUS, nullptr, sensitivityVectors->data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
                L"Unable to initialize the SUNDIALS solver."));
        checkFlag(CVodeSetSensParams(memory.get(), problem.sensitivityParameters.data(),
                      problem.sensitivityScales.data(), problem.sensitivityParameterList.data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
        checkFlag(CVodeSensEEtolerances(memory.get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
        checkFlag(CVodeSetSensErrCon(memory.get(), SUNTRUE),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    setCvodesTolerances(memory.get(), problem, sunctx.get());
    constraints = nonNegativeConstraints(problem, sunctx.get());
    if (constraints != nullptr) {
        checkFlag(CVodeSetConstraints(memory.get(), constraints->get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    checkFlag(CVodeSetUserData(memory.get(), &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    attachLinearSolver(
        problem, y.get(), sunctx.get(), linearSolverName, preconditionerName, linearSolverHandles,
        [&](SUNLinearSolver linearSolver, SUNMatrix jacobianMatrix) {
            checkFlag(CVodeSetLinearSolver(memory.get(), linearSolver, jacobianMatrix),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
        },
        [&]() {
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
            checkFlag(CVodeSetJacFn(memory.get(), cvodesSparseDirectJacobian),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
#endif
        },
        [&](const std::string& name) {
            CVLsPrecSetupFn setup = name == "jacobi"
                ? cvodesJacobiSetup
                : (name == "banded" ? cvodesBandedSetup : cvodesIlu0Setup);
            CVLsPrecSolveFn solve = name == "jacobi"
                ? cvodesJacobiSolve
                : (name == "banded" ? cvodesBandedSolve : cvodesIlu0Solve);
            checkFlag(CVodeSetPreconditioner(memory.get(), setup, solve),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
        });
    ArrayOf maxOrder = getOptionalField(problem.options, "MaxOrder");
    if (!maxOrder.isEmpty()) {
        checkFlag(CVodeSetMaxOrd(memory.get(), static_cast<int>(maxOrder.asDoubleScalar())),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    checkFlag(CVodeSetInitStep(
                  memory.get(), getOptionalField(problem.options, "InitialStep").asDoubleScalar()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(CVodeSetMaxStep(
                  memory.get(), getOptionalField(problem.options, "MaxStep").asDoubleScalar()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    ArrayOf minStep = getOptionalField(problem.options, "MinStep");
    if (!minStep.isEmpty()) {
        checkFlag(CVodeSetMinStep(memory.get(), minStep.asDoubleScalar()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    initializeEvents(problem, tspan.front(), y0.data(), nullptr);
    if (problem.eventData.count > 0) {
        checkFlag(CVodeRootInit(memory.get(), problem.eventData.count, cvodesRoot),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
                L"Unable to initialize the SUNDIALS solver."));
        checkFlag(CVodeSetRootDirection(memory.get(), problem.eventData.direction.data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }

    std::vector<double> x { outputTimes.front() };
    std::vector<double> values;
    appendColumn(values, y.data(), problem.n);
    if (cvodesRhs(tspan.front(), y.get(), yd.get(), &problem) < 0) {
        if (!problem.callbackError.empty()) {
            RaiseError(problem.callbackError, L"Nelson:ode:callbackError");
        }
        Error(L"Nelson:ode_solvers:initialSlopeFailed", L"Unable to evaluate initial slope.");
    }
    std::vector<double> slopes;
    appendColumn(slopes, yd.data(), problem.n);
    std::vector<double> sensitivityValues;
    if (problem.useNativeSensitivity) {
        appendSensitivity(
            sensitivityValues, sensitivityVectors->data(), problem.n, problem.sensitivityCount);
    }
    std::vector<double> xe;
    std::vector<double> ye;
    std::vector<double> ie;
    bool stop = false;
    double currentT = outputTimes.front();
    for (size_t k = 1; k < outputTimes.size() && !stop; ++k) {
        while (
            (outputTimes[k] - currentT) * (outputTimes.back() - outputTimes.front()) > 0 && !stop) {
            double tret = currentT;
            // A recoverable callback failure may have stored a message on a
            // retried step; clear it so a later hard failure reports its own
            // cause instead of a stale one.
            problem.callbackError.clear();
            const int flag = CVode(memory.get(), outputTimes[k], y.get(), &tret, CV_NORMAL);
            currentT = tret;
            if (flag < 0) {
                if (!problem.callbackError.empty()) {
                    RaiseError(problem.callbackError, L"Nelson:ode:callbackError");
                }
                Error(L"Nelson:ode_solvers:sundialsIntegrationFailed",
                    L"SUNDIALS integration failed.");
            }
            if (flag == CV_ROOT_RETURN) {
                std::vector<int> roots(problem.eventData.count, 0);
                checkFlag(CVodeGetRootInfo(memory.get(), roots.data()),
                    Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                        L"Unable to retrieve data from the SUNDIALS solver."));
                for (int root = 0; root < problem.eventData.count; ++root) {
                    if (roots[root] != 0) {
                        appendEvent(xe, ye, ie, y.data(), problem.n, currentT, root);
                        stop = stop || problem.eventData.terminal[root] != 0;
                    }
                }
            } else {
                break;
            }
        }
        x.push_back(currentT);
        appendColumn(values, y.data(), problem.n);
        checkFlag(CVodeGetDky(memory.get(), currentT, 1, yd.get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                L"Unable to retrieve data from the SUNDIALS solver."));
        appendColumn(slopes, yd.data(), problem.n);
        if (problem.useNativeSensitivity) {
            double sensT = currentT;
            checkFlag(CVodeGetSens(memory.get(), &sensT, sensitivityVectors->data()),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                    L"Unable to retrieve data from the SUNDIALS solver."));
            appendSensitivity(
                sensitivityValues, sensitivityVectors->data(), problem.n, problem.sensitivityCount);
        }
    }
    long int nsteps = 0;
    long int nfevals = 0;
    long int nfailed = 0;
    // Statistics getters may fail harmlessly; the counters stay at zero.
    (void)CVodeGetNumSteps(memory.get(), &nsteps);
    (void)CVodeGetNumRhsEvals(memory.get(), &nfevals);
    (void)CVodeGetNumErrTestFails(memory.get(), &nfailed);
    ArrayOf stats = buildStats(nsteps, nfevals, currentT, linearSolverName, preconditionerName,
        problem.useNativeSensitivity ? "native" : "none", cvodesLinearStats(memory.get(), problem),
        nfailed);
    return buildSolution(problem, x, values, slopes, xe, ye, ie, stats, sensitivityValues);
}
//=============================================================================
ArrayOf
runIdas(NativeProblem& problem, const std::vector<double>& tspan, const std::vector<double>& y0,
    const std::vector<double>& yp0)
{
    if (problem.useAdjointSensitivity) {
        return runIdasAdjoint(problem, tspan, y0, yp0);
    }
    const std::vector<double> outputTimes = solverOutputTimes(problem, tspan);
    SunContextHandle sunctx;
    SerialVectorHandle y(problem.n, sunctx.get());
    SerialVectorHandle yp(problem.n, sunctx.get());
    IdasMemoryHandle memory(sunctx.get());
    std::unique_ptr<SerialVectorArrayHandle> sensitivityVectors;
    std::unique_ptr<SerialVectorArrayHandle> sensitivitySlopeVectors;
    std::unique_ptr<SerialVectorHandle> constraints;
    std::string linearSolverName = linearSolverNameForProblem(problem);
    std::string preconditionerName = preconditionerNameForProblem(problem, linearSolverName);
    LinearSolverHandles linearSolverHandles;
    std::copy(y0.begin(), y0.end(), y.data());
    std::copy(yp0.begin(), yp0.end(), yp.data());
    checkFlag(IDAInit(memory.get(), idasResidual, tspan.front(), y.get(), yp.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    if (problem.useNativeSensitivity) {
        sensitivityVectors = std::make_unique<SerialVectorArrayHandle>(
            problem.sensitivityCount, problem.n, problem.sensitivityInitialValue, sunctx.get());
        sensitivitySlopeVectors = std::make_unique<SerialVectorArrayHandle>(
            problem.sensitivityCount, problem.n, problem.sensitivityInitialSlope, sunctx.get());
        checkFlag(
            IDASensInit(memory.get(), static_cast<int>(problem.sensitivityCount), IDA_SIMULTANEOUS,
                nullptr, sensitivityVectors->data(), sensitivitySlopeVectors->data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
                L"Unable to initialize the SUNDIALS solver."));
        checkFlag(IDASetSensParams(memory.get(), problem.sensitivityParameters.data(),
                      problem.sensitivityScales.data(), problem.sensitivityParameterList.data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
        checkFlag(IDASensEEtolerances(memory.get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
        checkFlag(IDASetSensErrCon(memory.get(), SUNTRUE),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    setIdasTolerances(memory.get(), problem, sunctx.get());
    constraints = nonNegativeConstraints(problem, sunctx.get());
    if (constraints != nullptr) {
        checkFlag(IDASetConstraints(memory.get(), constraints->get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    checkFlag(IDASetUserData(memory.get(), &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    attachLinearSolver(
        problem, y.get(), sunctx.get(), linearSolverName, preconditionerName, linearSolverHandles,
        [&](SUNLinearSolver linearSolver, SUNMatrix jacobianMatrix) {
            checkFlag(IDASetLinearSolver(memory.get(), linearSolver, jacobianMatrix),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
        },
        [&]() {
#if defined(NELSON_WITH_SUNDIALS_KLU) && defined(NELSON_WITH_SUNDIALS_SPARSE_MATRIX)
            checkFlag(IDASetJacFn(memory.get(), idasSparseDirectJacobian),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
#endif
        },
        [&](const std::string& name) {
            IDALsPrecSetupFn setup = name == "jacobi"
                ? idasJacobiSetup
                : (name == "banded" ? idasBandedSetup : idasIlu0Setup);
            IDALsPrecSolveFn solve = name == "jacobi"
                ? idasJacobiSolve
                : (name == "banded" ? idasBandedSolve : idasIlu0Solve);
            checkFlag(IDASetPreconditioner(memory.get(), setup, solve),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
        });
    ArrayOf maxOrder = getOptionalField(problem.options, "MaxOrder");
    if (!maxOrder.isEmpty()) {
        checkFlag(IDASetMaxOrd(memory.get(), static_cast<int>(maxOrder.asDoubleScalar())),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    checkFlag(IDASetInitStep(
                  memory.get(), getOptionalField(problem.options, "InitialStep").asDoubleScalar()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(
        IDASetMaxStep(memory.get(), getOptionalField(problem.options, "MaxStep").asDoubleScalar()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    ArrayOf minStep = getOptionalField(problem.options, "MinStep");
    if (!minStep.isEmpty()) {
        checkFlag(IDASetMinStep(memory.get(), minStep.asDoubleScalar()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }
    initializeEvents(problem, tspan.front(), y0.data(), yp0.data());
    if (problem.eventData.count > 0) {
        checkFlag(IDARootInit(memory.get(), problem.eventData.count, idasRoot),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
                L"Unable to initialize the SUNDIALS solver."));
        checkFlag(IDASetRootDirection(memory.get(), problem.eventData.direction.data()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                L"Unable to configure the SUNDIALS solver."));
    }

    std::vector<double> x { outputTimes.front() };
    std::vector<double> values;
    appendColumn(values, y.data(), problem.n);
    std::vector<double> slopes;
    appendColumn(slopes, yp.data(), problem.n);
    std::vector<double> sensitivityValues;
    if (problem.useNativeSensitivity) {
        appendSensitivity(
            sensitivityValues, sensitivityVectors->data(), problem.n, problem.sensitivityCount);
    }
    std::vector<double> xe;
    std::vector<double> ye;
    std::vector<double> ie;
    bool stop = false;
    double currentT = outputTimes.front();
    for (size_t k = 1; k < outputTimes.size() && !stop; ++k) {
        while (
            (outputTimes[k] - currentT) * (outputTimes.back() - outputTimes.front()) > 0 && !stop) {
            double tret = currentT;
            checkFlag(IDASetStopTime(memory.get(), outputTimes[k]),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
                    L"Unable to configure the SUNDIALS solver."));
            // Clear any message stored by a recoverable callback failure on a
            // retried step so a later hard failure reports its own cause.
            problem.callbackError.clear();
            const int flag
                = IDASolve(memory.get(), outputTimes[k], &tret, y.get(), yp.get(), IDA_NORMAL);
            currentT = tret;
            if (flag < 0) {
                if (!problem.callbackError.empty()) {
                    RaiseError(problem.callbackError, L"Nelson:ode:callbackError");
                }
                Error(L"Nelson:ode_solvers:sundialsIntegrationFailed",
                    L"SUNDIALS integration failed.");
            }
            if (flag == IDA_ROOT_RETURN) {
                std::vector<int> roots(problem.eventData.count, 0);
                checkFlag(IDAGetRootInfo(memory.get(), roots.data()),
                    Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                        L"Unable to retrieve data from the SUNDIALS solver."));
                for (int root = 0; root < problem.eventData.count; ++root) {
                    if (roots[root] != 0) {
                        appendEvent(xe, ye, ie, y.data(), problem.n, currentT, root);
                        stop = stop || problem.eventData.terminal[root] != 0;
                    }
                }
            } else {
                break;
            }
        }
        x.push_back(currentT);
        appendColumn(values, y.data(), problem.n);
        appendColumn(slopes, yp.data(), problem.n);
        if (problem.useNativeSensitivity) {
            double sensT = currentT;
            checkFlag(IDAGetSens(memory.get(), &sensT, sensitivityVectors->data()),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                    L"Unable to retrieve data from the SUNDIALS solver."));
            appendSensitivity(
                sensitivityValues, sensitivityVectors->data(), problem.n, problem.sensitivityCount);
        }
    }
    long int nsteps = 0;
    long int nfevals = 0;
    long int nfailed = 0;
    // Statistics getters may fail harmlessly; the counters stay at zero.
    (void)IDAGetNumSteps(memory.get(), &nsteps);
    (void)IDAGetNumResEvals(memory.get(), &nfevals);
    (void)IDAGetNumErrTestFails(memory.get(), &nfailed);
    ArrayOf stats = buildStats(nsteps, nfevals, currentT, linearSolverName, preconditionerName,
        problem.useNativeSensitivity ? "native" : "none", idasLinearStats(memory.get(), problem),
        nfailed);
    return buildSolution(problem, x, values, slopes, xe, ye, ie, stats, sensitivityValues);
}
#endif
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::OdeSolversGateway::odeSundialsRunBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
#ifndef NELSON_WITH_SUNDIALS
    Error(L"Nelson:ode_solvers:sundialsBackendNotAvailable", L"SUNDIALS backend is not available.");
    return {};
#else
    if (!argIn[0].isStruct()) {
        Error(L"Nelson:ode:problemStructExpected", L"ODE problem structure expected.");
    }
    NativeProblem problem;
    problem.eval = eval;
    problem.solver = argIn[0].field(FIELD_SOLVER).asUtf8String();
    problem.odefun = argIn[0].field(FIELD_ODEFUN);
    problem.options = argIn[0].field(FIELD_OPTIONS);
    problem.mass = getOptionalField(problem.options, "Mass");
    problem.events = getOptionalField(problem.options, "Events");
    problem.eventDefinitionObject = getOptionalField(problem.options, "EventDefinitionObject");
    problem.parameters = cellValues(getOptionalField(problem.options, "Parameters"));
    std::vector<double> tspan = realDoubleVector(argIn[0].field(FIELD_TSPAN),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
            L"Time span"));
    std::vector<double> y0 = realDoubleVector(argIn[0].field(FIELD_Y0),
        formatI18nMessage(
            resolveMessageTemplate(L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
            L"Initial value"));
    problem.n = static_cast<indexType>(y0.size());
    if (tspan.size() < 2 || problem.n == 0) {
        Error(L"Nelson:ode:invalidProblem", L"Invalid ODE problem.");
    }
    initializeSensitivityProblem(problem);
    if (problem.solver == "idas") {
        ArrayOf yp0Field = argIn[0].field(FIELD_YP0);
        if (yp0Field.isEmpty()) {
            yp0Field = getOptionalField(problem.options, "InitialSlope");
        }
        std::vector<double> yp0(problem.n, 0);
        if (!yp0Field.isEmpty()) {
            yp0 = realDoubleVector(yp0Field,
                formatI18nMessage(resolveMessageTemplate(
                                      L"Nelson:ode_solvers:realDouble", L"%s must be real double."),
                    L"Initial slope"));
        }
        if (yp0.size() != static_cast<size_t>(problem.n)) {
            Error(L"Nelson:dimagree:initialSlopeHasIncompatibleSize",
                L"Initial slope has incompatible size.");
        }
        problem.yp0 = doubleColumn(yp0.data(), problem.n);
        ArrayOfVector retval;
        retval << runIdas(problem, tspan, y0, yp0);
        return retval;
    }
    problem.yp0 = ArrayOf::empty();
    ArrayOfVector retval;
    retval << runCvodes(problem, tspan, y0);
    return retval;
#endif
}
//=============================================================================
