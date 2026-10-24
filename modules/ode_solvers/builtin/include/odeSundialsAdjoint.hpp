//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
// Implementation fragment: this header defines non-inline functions and is
// meant to be included exactly once, inside the anonymous namespace of
// odeSundialsRunBuiltin.cpp. Do not include it from any other translation
// unit.
//=============================================================================
double
adjointScalarValue(NativeProblem* problem, const ArrayOf& fcn, double t, const double* y)
{
    if (fcn.isEmpty()) {
        return 0;
    }
    ArrayOfVector out
        = callProblemFunction(problem->eval, fcn, t, y, problem->n, currentParameters(problem), 1);
    if (out.empty()) {
        Error(L"Nelson:ode_solvers:adjointScalarNoValue",
            L"Adjoint scalar function returned no value.");
    }
    std::vector<double> value = realDoubleVector(out[0],
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:adjointScalarFunctionReturn",
            L"Adjoint scalar function must return real double."));
    if (value.size() != 1) {
        Error(L"Nelson:ode_solvers:adjointScalarNotScalar",
            L"Adjoint scalar function must return a scalar.");
    }
    return value[0];
}
//=============================================================================
std::vector<double>
adjointRhsValue(NativeProblem* problem, double t, const double* y)
{
    ArrayOfVector out = callProblemFunction(
        problem->eval, problem->odefun, t, y, problem->n, currentParameters(problem), 1);
    if (out.empty()) {
        Error(L"Nelson:ode_solvers:odeFunctionNoValue", L"ODE function returned no value.");
    }
    std::vector<double> rhs = realDoubleVector(out[0],
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:odeFunctionReturnDoubles", L"ODE function must return doubles."));
    if (rhs.size() != static_cast<size_t>(problem->n)) {
        Error(L"Nelson:ode_solvers:odeFunctionSizeMismatch",
            L"ODE function returned a vector with incompatible size.");
    }
    return applyMass(problem, t, y, rhs);
}
//=============================================================================
bool
adjointCanUseProvidedJacobian(NativeProblem* problem)
{
    ArrayOf jacobian = getOptionalField(problem->options, "Jacobian");
    if (jacobian.isEmpty()) {
        return false;
    }
    if (problem->mass.isEmpty()) {
        return true;
    }
    ArrayOf stateDependence = getOptionalField(problem->options, "MStateDependence");
    return stateDependence.isEmpty() || stateDependence.asWideString() == L"none";
}
//=============================================================================
std::vector<double>
adjointProvidedStateJacobian(NativeProblem* problem, double t, const double* y)
{
    ArrayOf jacobian = getOptionalField(problem->options, "Jacobian");
    std::vector<double> values;
    if (jacobian.isFunctionHandle()) {
        ArrayOfVector out = callProblemFunction(
            problem->eval, jacobian, t, y, problem->n, currentParameters(problem), 1);
        if (out.empty()) {
            Error(L"Nelson:ode_solvers:jacobianNoValue", L"Jacobian function returned no value.");
        }
        values = realDoubleVector(out[0],
            Nelson::resolveMessageTemplate(
                L"Nelson:ode_solvers:jacobianRealDouble", L"Jacobian value must be real double."));
    } else {
        values = realDoubleVector(jacobian,
            Nelson::resolveMessageTemplate(
                L"Nelson:ode_solvers:jacobianRealDouble", L"Jacobian value must be real double."));
    }
    if (values.size() != static_cast<size_t>(problem->n * problem->n)) {
        Error(
            L"Nelson:ode_solvers:jacobianSizeMismatch", L"Jacobian matrix has incompatible size.");
    }
    if (problem->mass.isEmpty()) {
        return values;
    }
    std::vector<double> effective(values.size(), 0);
    std::vector<double> column(static_cast<size_t>(problem->n), 0);
    for (indexType col = 0; col < problem->n; ++col) {
        for (indexType row = 0; row < problem->n; ++row) {
            column[static_cast<size_t>(row)] = values[static_cast<size_t>(row + col * problem->n)];
        }
        std::vector<double> solved = applyMass(problem, t, y, column);
        for (indexType row = 0; row < problem->n; ++row) {
            effective[static_cast<size_t>(row + col * problem->n)]
                = solved[static_cast<size_t>(row)];
        }
    }
    return effective;
}
//=============================================================================
std::vector<double>
adjointStateGradient(
    NativeProblem* problem, const ArrayOf& fcn, double t, const double* y, double baseValue)
{
    std::vector<double> gradient(static_cast<size_t>(problem->n), 0);
    if (fcn.isEmpty()) {
        return gradient;
    }
    std::vector<double> trial(y, y + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        const double value = adjointScalarValue(problem, fcn, t, trial.data());
        gradient[idx] = (value - baseValue) / step;
        trial[idx] = saved;
    }
    return gradient;
}
//=============================================================================
std::vector<double>
adjointParameterGradient(
    NativeProblem* problem, const ArrayOf& fcn, double t, const double* y, double baseValue)
{
    std::vector<double> gradient(static_cast<size_t>(problem->sensitivityCount), 0);
    if (fcn.isEmpty()) {
        return gradient;
    }
    for (indexType k = 0; k < problem->sensitivityCount; ++k) {
        const int parameterIndex = problem->sensitivityParameterList[static_cast<size_t>(k)];
        const size_t idx = static_cast<size_t>(parameterIndex);
        const double saved = problem->sensitivityParameters[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        problem->sensitivityParameters[idx] = saved + step;
        const double value = adjointScalarValue(problem, fcn, t, y);
        gradient[static_cast<size_t>(k)] = (value - baseValue) / step;
        problem->sensitivityParameters[idx] = saved;
    }
    return gradient;
}
//=============================================================================
std::vector<double>
adjointRhsStateJacobian(
    NativeProblem* problem, double t, const double* y, const std::vector<double>& baseRhs)
{
    if (adjointCanUseProvidedJacobian(problem)) {
        return adjointProvidedStateJacobian(problem, t, y);
    }
    std::vector<double> jacobian(static_cast<size_t>(problem->n * problem->n), 0);
    std::vector<double> trial(y, y + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        const std::vector<double> rhs = adjointRhsValue(problem, t, trial.data());
        for (indexType row = 0; row < problem->n; ++row) {
            jacobian[static_cast<size_t>(row + k * problem->n)]
                = (rhs[static_cast<size_t>(row)] - baseRhs[static_cast<size_t>(row)]) / step;
        }
        trial[idx] = saved;
    }
    return jacobian;
}
//=============================================================================
std::vector<double>
adjointRhsParameterJacobian(
    NativeProblem* problem, double t, const double* y, const std::vector<double>& baseRhs)
{
    std::vector<double> jacobian(static_cast<size_t>(problem->n * problem->sensitivityCount), 0);
    for (indexType k = 0; k < problem->sensitivityCount; ++k) {
        const int parameterIndex = problem->sensitivityParameterList[static_cast<size_t>(k)];
        const size_t idx = static_cast<size_t>(parameterIndex);
        const double saved = problem->sensitivityParameters[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        problem->sensitivityParameters[idx] = saved + step;
        const std::vector<double> rhs = adjointRhsValue(problem, t, y);
        for (indexType row = 0; row < problem->n; ++row) {
            jacobian[static_cast<size_t>(row + k * problem->n)]
                = (rhs[static_cast<size_t>(row)] - baseRhs[static_cast<size_t>(row)]) / step;
        }
        problem->sensitivityParameters[idx] = saved;
    }
    return jacobian;
}
//=============================================================================
std::vector<double>
idasAdjointResidualValue(NativeProblem* problem, double t, const double* y, const double* yp)
{
    ArrayOfVector out = callImplicitFunction(
        problem->eval, problem->odefun, t, y, yp, problem->n, currentParameters(problem), 1);
    if (out.empty()) {
        Error(
            L"Nelson:ode_solvers:implicitFunctionNoValue", L"Implicit function returned no value.");
    }
    std::vector<double> residual = realDoubleVector(out[0],
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
            L"Implicit function must return doubles."));
    if (residual.size() != static_cast<size_t>(problem->n)) {
        Error(L"Nelson:ode_solvers:implicitFunctionSizeMismatch",
            L"Implicit function returned a vector with incompatible size.");
    }
    return residual;
}
//=============================================================================
std::vector<double>
transposeDense(const std::vector<double>& values, indexType n)
{
    std::vector<double> result(values.size(), 0);
    for (indexType col = 0; col < n; ++col) {
        for (indexType row = 0; row < n; ++row) {
            result[static_cast<size_t>(row + col * n)] = values[static_cast<size_t>(col + row * n)];
        }
    }
    return result;
}
//=============================================================================
std::vector<double>
idasAdjointResidualStateJacobian(NativeProblem* problem, double t, const double* y,
    const double* yp, const std::vector<double>& baseResidual)
{
    std::vector<double> jacobian(static_cast<size_t>(problem->n * problem->n), 0);
    std::vector<double> trial(y, y + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        const std::vector<double> residual = idasAdjointResidualValue(problem, t, trial.data(), yp);
        for (indexType row = 0; row < problem->n; ++row) {
            jacobian[static_cast<size_t>(row + k * problem->n)]
                = (residual[static_cast<size_t>(row)] - baseResidual[static_cast<size_t>(row)])
                / step;
        }
        trial[idx] = saved;
    }
    return jacobian;
}
//=============================================================================
std::vector<double>
idasAdjointResidualSlopeJacobian(NativeProblem* problem, double t, const double* y,
    const double* yp, const std::vector<double>& baseResidual)
{
    std::vector<double> jacobian(static_cast<size_t>(problem->n * problem->n), 0);
    std::vector<double> trial(yp, yp + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        const std::vector<double> residual = idasAdjointResidualValue(problem, t, y, trial.data());
        for (indexType row = 0; row < problem->n; ++row) {
            jacobian[static_cast<size_t>(row + k * problem->n)]
                = (residual[static_cast<size_t>(row)] - baseResidual[static_cast<size_t>(row)])
                / step;
        }
        trial[idx] = saved;
    }
    return jacobian;
}
//=============================================================================
std::vector<double>
idasAdjointResidualParameterJacobian(NativeProblem* problem, double t, const double* y,
    const double* yp, const std::vector<double>& baseResidual)
{
    std::vector<double> jacobian(static_cast<size_t>(problem->n * problem->sensitivityCount), 0);
    for (indexType k = 0; k < problem->sensitivityCount; ++k) {
        const int parameterIndex = problem->sensitivityParameterList[static_cast<size_t>(k)];
        const size_t idx = static_cast<size_t>(parameterIndex);
        const double saved = problem->sensitivityParameters[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        problem->sensitivityParameters[idx] = saved + step;
        const std::vector<double> residual = idasAdjointResidualValue(problem, t, y, yp);
        for (indexType row = 0; row < problem->n; ++row) {
            jacobian[static_cast<size_t>(row + k * problem->n)]
                = (residual[static_cast<size_t>(row)] - baseResidual[static_cast<size_t>(row)])
                / step;
        }
        problem->sensitivityParameters[idx] = saved;
    }
    return jacobian;
}
//=============================================================================
std::vector<double>
idasAdjointFinalSlope(NativeProblem* problem, double t, const double* y, const double* yp,
    const std::vector<double>& lambda, const std::vector<double>& objectiveGradient)
{
    const std::vector<double> residual = idasAdjointResidualValue(problem, t, y, yp);
    const std::vector<double> fy = idasAdjointResidualStateJacobian(problem, t, y, yp, residual);
    const std::vector<double> fyp = idasAdjointResidualSlopeJacobian(problem, t, y, yp, residual);
    std::vector<double> rhs(static_cast<size_t>(problem->n), 0);
    for (indexType col = 0; col < problem->n; ++col) {
        double value = -objectiveGradient[static_cast<size_t>(col)];
        for (indexType row = 0; row < problem->n; ++row) {
            value += fy[static_cast<size_t>(row + col * problem->n)]
                * lambda[static_cast<size_t>(row)];
        }
        rhs[static_cast<size_t>(col)] = value;
    }
    return solveDense(transposeDense(fyp, problem->n), rhs);
}
//=============================================================================
int
idasAdjointResidual(
    sunrealtype t, N_Vector y, N_Vector yp, N_Vector yB, N_Vector ypB, N_Vector rB, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        const std::vector<double> residual
            = idasAdjointResidualValue(problem, t, NV_DATA_S(y), NV_DATA_S(yp));
        const std::vector<double> fy
            = idasAdjointResidualStateJacobian(problem, t, NV_DATA_S(y), NV_DATA_S(yp), residual);
        const std::vector<double> fyp
            = idasAdjointResidualSlopeJacobian(problem, t, NV_DATA_S(y), NV_DATA_S(yp), residual);
        const double q = adjointScalarValue(problem, problem->adjointQuadrature, t, NV_DATA_S(y));
        const std::vector<double> qy
            = adjointStateGradient(problem, problem->adjointQuadrature, t, NV_DATA_S(y), q);
        for (indexType col = 0; col < problem->n; ++col) {
            double value = qy[static_cast<size_t>(col)];
            for (indexType row = 0; row < problem->n; ++row) {
                value -= fy[static_cast<size_t>(row + col * problem->n)] * NV_DATA_S(yB)[row];
                value += fyp[static_cast<size_t>(row + col * problem->n)] * NV_DATA_S(ypB)[row];
            }
            NV_DATA_S(rB)[col] = value;
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasAdjointQuadratureRhs(sunrealtype t, N_Vector y, N_Vector yp, N_Vector yB, N_Vector ypB,
    N_Vector qBdot, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        const std::vector<double> residual
            = idasAdjointResidualValue(problem, t, NV_DATA_S(y), NV_DATA_S(yp));
        const std::vector<double> fp = idasAdjointResidualParameterJacobian(
            problem, t, NV_DATA_S(y), NV_DATA_S(yp), residual);
        const double q = adjointScalarValue(problem, problem->adjointQuadrature, t, NV_DATA_S(y));
        const std::vector<double> qp
            = adjointParameterGradient(problem, problem->adjointQuadrature, t, NV_DATA_S(y), q);
        for (indexType col = 0; col < problem->sensitivityCount; ++col) {
            double value = -qp[static_cast<size_t>(col)];
            for (indexType row = 0; row < problem->n; ++row) {
                value += fp[static_cast<size_t>(row + col * problem->n)] * NV_DATA_S(yB)[row];
            }
            NV_DATA_S(qBdot)[col] = value;
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesAdjointRhs(sunrealtype t, N_Vector y, N_Vector yB, N_Vector yBdot, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        const std::vector<double> rhs = adjointRhsValue(problem, t, NV_DATA_S(y));
        const std::vector<double> jac = adjointRhsStateJacobian(problem, t, NV_DATA_S(y), rhs);
        const double q = adjointScalarValue(problem, problem->adjointQuadrature, t, NV_DATA_S(y));
        const std::vector<double> qy
            = adjointStateGradient(problem, problem->adjointQuadrature, t, NV_DATA_S(y), q);
        for (indexType col = 0; col < problem->n; ++col) {
            double value = -qy[static_cast<size_t>(col)];
            for (indexType row = 0; row < problem->n; ++row) {
                value -= jac[static_cast<size_t>(row + col * problem->n)] * NV_DATA_S(yB)[row];
            }
            NV_DATA_S(yBdot)[col] = value;
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesAdjointQuadratureRhs(sunrealtype t, N_Vector y, N_Vector yB, N_Vector qBdot, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        const std::vector<double> rhs = adjointRhsValue(problem, t, NV_DATA_S(y));
        const std::vector<double> fp = adjointRhsParameterJacobian(problem, t, NV_DATA_S(y), rhs);
        const double q = adjointScalarValue(problem, problem->adjointQuadrature, t, NV_DATA_S(y));
        const std::vector<double> qp
            = adjointParameterGradient(problem, problem->adjointQuadrature, t, NV_DATA_S(y), q);
        for (indexType col = 0; col < problem->sensitivityCount; ++col) {
            double value = -qp[static_cast<size_t>(col)];
            for (indexType row = 0; row < problem->n; ++row) {
                value -= fp[static_cast<size_t>(row + col * problem->n)] * NV_DATA_S(yB)[row];
            }
            NV_DATA_S(qBdot)[col] = value;
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
ArrayOf
runCvodesAdjoint(
    NativeProblem& problem, const std::vector<double>& tspan, const std::vector<double>& y0)
{
    if (problem.eventData.count > 0 || !problem.events.isEmpty()) {
        Error(L"Nelson:ode_solvers:adjointEventsUnsupported",
            L"Adjoint sensitivities with events are not supported yet.");
    }
    if (!std::isnan(problem.adjointObjectiveTime)
        && std::abs(problem.adjointObjectiveTime - tspan.back()) > 100
                * std::numeric_limits<double>::epsilon() * std::max(1.0, std::abs(tspan.back()))) {
        Error(L"Nelson:ode_solvers:adjointFinalTimeOnly",
            L"Only final-time adjoint objectives are supported yet.");
    }
    const std::vector<double> outputTimes = solverOutputTimes(problem, tspan);
    SunContextHandle sunctx;
    SerialVectorHandle y(problem.n, sunctx.get());
    SerialVectorHandle yd(problem.n, sunctx.get());
    CvodesMemoryHandle memory(problem.solver == "cvodesnonstiff" ? CV_ADAMS : CV_BDF, sunctx.get());
    std::copy(y0.begin(), y0.end(), y.data());
    checkFlag(CVodeInit(memory.get(), cvodesRhs, tspan.front(), y.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    setCvodesTolerances(memory.get(), problem, sunctx.get());
    checkFlag(CVodeSetUserData(memory.get(), &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    DenseMatrixHandle matrix(problem.n, problem.n, sunctx.get());
    DenseLinearSolverHandle linearSolver(y.get(), matrix.get(), sunctx.get());
    checkFlag(CVodeSetLinearSolver(memory.get(), linearSolver.get(), matrix.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(CVodeAdjInit(memory.get(), 100, CV_HERMITE),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    std::vector<double> x { outputTimes.front() };
    std::vector<double> values;
    appendColumn(values, y.data(), problem.n);
    if (cvodesRhs(tspan.front(), y.get(), yd.get(), &problem) < 0) {
        if (!problem.callbackError.empty()) {
            RaiseError(problem.callbackError, L"Nelson:ode_solvers:callbackFailed");
        }
        Error(L"Nelson:ode_solvers:initialSlopeFailed", L"Unable to evaluate initial slope.");
    }
    std::vector<double> slopes;
    appendColumn(slopes, yd.data(), problem.n);
    double currentT = outputTimes.front();
    int checkpoints = 0;
    for (size_t k = 1; k < outputTimes.size(); ++k) {
        double tret = currentT;
        // Clear any message stored by a recoverable callback failure on a
        // retried step so a later hard failure reports its own cause.
        problem.callbackError.clear();
        const int flag
            = CVodeF(memory.get(), outputTimes[k], y.get(), &tret, CV_NORMAL, &checkpoints);
        currentT = tret;
        if (flag < 0) {
            if (!problem.callbackError.empty()) {
                RaiseError(problem.callbackError, L"Nelson:ode_solvers:callbackFailed");
            }
            Error(L"Nelson:ode_solvers:cvodesForwardFailed", L"SUNDIALS integration failed.");
        }
        x.push_back(currentT);
        appendColumn(values, y.data(), problem.n);
        checkFlag(CVodeGetDky(memory.get(), currentT, 1, yd.get()),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
                L"Unable to retrieve data from the SUNDIALS solver."));
        appendColumn(slopes, yd.data(), problem.n);
    }
    const double objective
        = adjointScalarValue(&problem, problem.adjointObjective, currentT, y.data());
    const std::vector<double> lambda0
        = adjointStateGradient(&problem, problem.adjointObjective, currentT, y.data(), objective);
    const std::vector<double> gradient0 = adjointParameterGradient(
        &problem, problem.adjointObjective, currentT, y.data(), objective);
    SerialVectorHandle yB(lambda0, sunctx.get());
    SerialVectorHandle qB(gradient0, sunctx.get());
    int which = 0;
    checkFlag(
        CVodeCreateB(memory.get(), problem.solver == "cvodesnonstiff" ? CV_ADAMS : CV_BDF, &which),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    checkFlag(CVodeInitB(memory.get(), which, cvodesAdjointRhs, currentT, yB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    const double relTol = std::isnan(problem.adjointRelativeTolerance)
        ? getOptionalField(problem.options, "RelTol").asDoubleScalar()
        : problem.adjointRelativeTolerance;
    const double absTol
        = std::isnan(problem.adjointAbsoluteTolerance) ? 1e-10 : problem.adjointAbsoluteTolerance;
    checkFlag(CVodeSStolerancesB(memory.get(), which, relTol, absTol),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(CVodeSetUserDataB(memory.get(), which, &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    DenseMatrixHandle matrixB(problem.n, problem.n, sunctx.get());
    DenseLinearSolverHandle linearSolverB(yB.get(), matrixB.get(), sunctx.get());
    checkFlag(CVodeSetLinearSolverB(memory.get(), which, linearSolverB.get(), matrixB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(CVodeQuadInitB(memory.get(), which, cvodesAdjointQuadratureRhs, qB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    checkFlag(CVodeQuadSStolerancesB(memory.get(), which, relTol, absTol),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(CVodeB(memory.get(), outputTimes.front(), CV_NORMAL),
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:sundialsIntegrationFailed", L"SUNDIALS integration failed."));
    double backwardTime = currentT;
    checkFlag(CVodeGetQuadB(memory.get(), which, &backwardTime, qB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
            L"Unable to retrieve data from the SUNDIALS solver."));
    std::vector<double> adjointGradient(
        NV_DATA_S(qB.get()), NV_DATA_S(qB.get()) + problem.sensitivityCount);
    long int nsteps = 0;
    long int nfevals = 0;
    CVodeGetNumSteps(memory.get(), &nsteps);
    CVodeGetNumRhsEvals(memory.get(), &nfevals);
    ArrayOf stats = buildStats(nsteps, nfevals, currentT, "dense", "none", "adjoint");
    return buildSolution(problem, x, values, slopes, {}, {}, {}, stats, {}, adjointGradient);
}
//=============================================================================
ArrayOf
runIdasAdjoint(NativeProblem& problem, const std::vector<double>& tspan,
    const std::vector<double>& y0, const std::vector<double>& yp0)
{
    if (problem.eventData.count > 0 || !problem.events.isEmpty()) {
        Error(L"Nelson:ode_solvers:adjointEventsUnsupported",
            L"Adjoint sensitivities with events are not supported yet.");
    }
    if (!std::isnan(problem.adjointObjectiveTime)
        && std::abs(problem.adjointObjectiveTime - tspan.back()) > 100
                * std::numeric_limits<double>::epsilon() * std::max(1.0, std::abs(tspan.back()))) {
        Error(L"Nelson:ode_solvers:adjointFinalTimeOnly",
            L"Only final-time adjoint objectives are supported yet.");
    }
    const std::vector<double> outputTimes = solverOutputTimes(problem, tspan);
    SunContextHandle sunctx;
    SerialVectorHandle y(problem.n, sunctx.get());
    SerialVectorHandle yp(problem.n, sunctx.get());
    IdasMemoryHandle memory(sunctx.get());
    std::copy(y0.begin(), y0.end(), y.data());
    std::copy(yp0.begin(), yp0.end(), yp.data());
    checkFlag(IDAInit(memory.get(), idasResidual, tspan.front(), y.get(), yp.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    setIdasTolerances(memory.get(), problem, sunctx.get());
    checkFlag(IDASetUserData(memory.get(), &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    DenseMatrixHandle matrix(problem.n, problem.n, sunctx.get());
    DenseLinearSolverHandle linearSolver(y.get(), matrix.get(), sunctx.get());
    checkFlag(IDASetLinearSolver(memory.get(), linearSolver.get(), matrix.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(IDAAdjInit(memory.get(), 100, IDA_HERMITE),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    std::vector<double> x { outputTimes.front() };
    std::vector<double> values;
    appendColumn(values, y.data(), problem.n);
    std::vector<double> slopes;
    appendColumn(slopes, yp.data(), problem.n);
    double currentT = outputTimes.front();
    int checkpoints = 0;
    for (size_t k = 1; k < outputTimes.size(); ++k) {
        double tret = currentT;
        // Clear any message stored by a recoverable callback failure on a
        // retried step so a later hard failure reports its own cause.
        problem.callbackError.clear();
        const int flag = IDASolveF(
            memory.get(), outputTimes[k], &tret, y.get(), yp.get(), IDA_NORMAL, &checkpoints);
        currentT = tret;
        if (flag < 0) {
            if (!problem.callbackError.empty()) {
                RaiseError(problem.callbackError, L"Nelson:ode_solvers:callbackFailed");
            }
            Error(L"Nelson:ode_solvers:idasForwardFailed", L"SUNDIALS integration failed.");
        }
        x.push_back(currentT);
        appendColumn(values, y.data(), problem.n);
        appendColumn(slopes, yp.data(), problem.n);
    }
    const double objective
        = adjointScalarValue(&problem, problem.adjointObjective, currentT, y.data());
    const std::vector<double> lambda0
        = adjointStateGradient(&problem, problem.adjointObjective, currentT, y.data(), objective);
    const std::vector<double> gradient0 = adjointParameterGradient(
        &problem, problem.adjointObjective, currentT, y.data(), objective);
    const std::vector<double> lambdaSlope0
        = idasAdjointFinalSlope(&problem, currentT, y.data(), yp.data(), lambda0,
            adjointStateGradient(&problem, problem.adjointQuadrature, currentT, y.data(),
                adjointScalarValue(&problem, problem.adjointQuadrature, currentT, y.data())));
    SerialVectorHandle yB(lambda0, sunctx.get());
    SerialVectorHandle ypB(lambdaSlope0, sunctx.get());
    SerialVectorHandle qB(gradient0, sunctx.get());
    int which = 0;
    checkFlag(IDACreateB(memory.get(), &which),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    checkFlag(IDAInitB(memory.get(), which, idasAdjointResidual, currentT, yB.get(), ypB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    const double relTol = std::isnan(problem.adjointRelativeTolerance)
        ? getOptionalField(problem.options, "RelTol").asDoubleScalar()
        : problem.adjointRelativeTolerance;
    const double absTol
        = std::isnan(problem.adjointAbsoluteTolerance) ? 1e-10 : problem.adjointAbsoluteTolerance;
    checkFlag(IDASStolerancesB(memory.get(), which, relTol, absTol),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(IDASetUserDataB(memory.get(), which, &problem),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    DenseMatrixHandle matrixB(problem.n, problem.n, sunctx.get());
    DenseLinearSolverHandle linearSolverB(yB.get(), matrixB.get(), sunctx.get());
    checkFlag(IDASetLinearSolverB(memory.get(), which, linearSolverB.get(), matrixB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(IDAQuadInitB(memory.get(), which, idasAdjointQuadratureRhs, qB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableInitializeSundialsSolver",
            L"Unable to initialize the SUNDIALS solver."));
    checkFlag(IDAQuadSStolerancesB(memory.get(), which, relTol, absTol),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(IDASetQuadErrConB(memory.get(), which, SUNTRUE),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableConfigureSundialsSolver",
            L"Unable to configure the SUNDIALS solver."));
    checkFlag(IDASolveB(memory.get(), outputTimes.front(), IDA_NORMAL),
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:sundialsIntegrationFailed", L"SUNDIALS integration failed."));
    double backwardTime = currentT;
    checkFlag(IDAGetQuadB(memory.get(), which, &backwardTime, qB.get()),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:unableRetrieveDataFrom",
            L"Unable to retrieve data from the SUNDIALS solver."));
    std::vector<double> adjointGradient(
        NV_DATA_S(qB.get()), NV_DATA_S(qB.get()) + problem.sensitivityCount);
    long int nsteps = 0;
    long int nfevals = 0;
    IDAGetNumSteps(memory.get(), &nsteps);
    IDAGetNumResEvals(memory.get(), &nfevals);
    ArrayOf stats = buildStats(nsteps, nfevals, currentT, "dense", "none", "adjoint");
    stats = withSensitivityBackend(stats, "idasAdjoint");
    return buildSolution(problem, x, values, slopes, {}, {}, {}, stats, {}, adjointGradient);
}
//=============================================================================
