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
std::vector<double>
massValue(NativeProblem* problem, double t, const double* y)
{
    if (problem->mass.isEmpty()) {
        return {};
    }
    if (problem->mass.isFunctionHandle()) {
        ArrayOfVector args;
        args << ArrayOf::doubleScalar(t);
        args << doubleColumn(y, problem->n);
        ArrayOfVector parameters = currentParameters(problem);
        for (const auto& parameter : parameters) {
            args << parameter;
        }
        ArrayOfVector out = callFunction(problem->eval, problem->mass, args, 1);
        if (out.empty()) {
            Error(L"Nelson:ode_solvers:massFunctionNoValue", L"Mass function returned no value.");
        }
        return realDoubleVector(out[0],
            Nelson::resolveMessageTemplate(
                L"Nelson:ode_solvers:massRealDouble", L"Mass value must be real double."));
    }
    return realDoubleVector(problem->mass,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:massRealDouble", L"Mass value must be real double."));
}
//=============================================================================
std::vector<double>
applyMass(NativeProblem* problem, double t, const double* y, std::vector<double> rhs)
{
    std::vector<double> mass = massValue(problem, t, y);
    if (mass.empty()) {
        return rhs;
    }
    if (mass.size() == 1) {
        const double scale = mass[0];
        if (scale == 0) {
            Error(L"Nelson:ode_solvers:massMatrixSingular", L"Mass matrix is singular.");
        }
        for (auto& value : rhs) {
            value /= scale;
        }
        return rhs;
    }
    if (mass.size() != static_cast<size_t>(problem->n * problem->n)) {
        Error(L"Nelson:ode_solvers:massMatrixSizeMismatch", L"Mass matrix has incompatible size.");
    }
    return solveDense(mass, rhs);
}
//=============================================================================
void
evaluateEvents(NativeProblem* problem, double t, const double* y, const double* yp,
    std::vector<double>& values, std::vector<int>* terminal, std::vector<int>* direction)
{
    ArrayOfVector out;
    if (!problem->eventDefinitionObject.isEmpty()) {
        ArrayOfVector args;
        args << problem->eventDefinitionObject;
        args << ArrayOf::doubleScalar(t);
        args << doubleColumn(y, problem->n);
        if (yp != nullptr) {
            args << doubleColumn(yp, problem->n);
        }
        ArrayOfVector parameters = currentParameters(problem);
        for (const auto& parameter : parameters) {
            args << parameter;
        }
        out = callFunction(problem->eval, problem->events, args, 3);
    } else if (yp == nullptr) {
        out = callProblemFunction(
            problem->eval, problem->events, t, y, problem->n, currentParameters(problem), 3);
    } else {
        out = callImplicitFunction(
            problem->eval, problem->events, t, y, yp, problem->n, currentParameters(problem), 3);
    }
    if (out.size() < 3) {
        Error(L"Nelson:ode_solvers:eventFunctionOutputs",
            L"Event function must return value, terminal flag, and direction.");
    }
    values = realDoubleVector(out[0],
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:eventRealDouble", L"Event values must be real double."));
    const std::vector<double> terminalValues = realNumericVector(out[1],
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:eventTerminalFlagsNumeric",
            L"Event terminal flags must be numeric or logical."),
        true);
    const std::vector<double> directionValues = realNumericVector(out[2],
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:eventDirectionsNumeric", L"Event directions must be numeric."),
        false);
    if (terminalValues.size() != values.size() || directionValues.size() != values.size()) {
        Error(L"Nelson:ode_solvers:eventOutputsSizeMismatch",
            L"Event outputs must have the same size.");
    }
    if (terminal != nullptr) {
        terminal->assign(values.size(), 0);
        for (size_t k = 0; k < terminalValues.size(); ++k) {
            (*terminal)[k] = terminalValues[k] != 0;
        }
    }
    if (direction != nullptr) {
        direction->assign(values.size(), 0);
        for (size_t k = 0; k < directionValues.size(); ++k) {
            if (directionValues[k] > 0) {
                (*direction)[k] = 1;
            } else if (directionValues[k] < 0) {
                (*direction)[k] = -1;
            }
        }
    }
}
//=============================================================================
int
cvodesRhs(sunrealtype t, N_Vector y, N_Vector ydot, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ArrayOfVector out = callProblemFunction(problem->eval, problem->odefun, t, NV_DATA_S(y),
            problem->n, currentParameters(problem), 1);
        if (out.empty()) {
            Error(L"Nelson:ode_solvers:odeFunctionNoValue", L"ODE function returned no value.");
        }
        std::vector<double> rhs = realDoubleVector(out[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:odeFunctionReturnDoubles",
                L"ODE function must return doubles."));
        if (rhs.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:ode_solvers:odeFunctionSizeMismatch",
                L"ODE function returned a vector with incompatible size.");
        }
        rhs = applyMass(problem, t, NV_DATA_S(y), rhs);
        std::copy(rhs.begin(), rhs.end(), NV_DATA_S(ydot));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesRoot(sunrealtype t, N_Vector y, sunrealtype* gout, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        std::vector<double> values;
        evaluateEvents(problem, t, NV_DATA_S(y), nullptr, values, nullptr, nullptr);
        std::copy(values.begin(), values.end(), gout);
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasResidual(sunrealtype t, N_Vector y, N_Vector yp, N_Vector rr, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ArrayOfVector out = callImplicitFunction(problem->eval, problem->odefun, t, NV_DATA_S(y),
            NV_DATA_S(yp), problem->n, currentParameters(problem), 1);
        if (out.empty()) {
            Error(L"Nelson:ode_solvers:implicitFunctionNoValue",
                L"Implicit function returned no value.");
        }
        std::vector<double> residual = realDoubleVector(out[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        if (residual.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:ode_solvers:implicitFunctionSizeMismatch",
                L"Implicit function returned a vector with incompatible size.");
        }
        std::copy(residual.begin(), residual.end(), NV_DATA_S(rr));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasRoot(sunrealtype t, N_Vector y, N_Vector yp, sunrealtype* gout, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        std::vector<double> values;
        evaluateEvents(problem, t, NV_DATA_S(y), NV_DATA_S(yp), values, nullptr, nullptr);
        std::copy(values.begin(), values.end(), gout);
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
