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
void
checkFlag(int flag, const std::wstring& message)
{
    if (flag < 0) {
        Error(L"Nelson:ode_solvers:solverError", message);
    }
}
//=============================================================================
void
appendColumn(std::vector<double>& values, const double* column, indexType rows)
{
    values.insert(values.end(), column, column + rows);
}
//=============================================================================
void
appendSensitivity(std::vector<double>& values, N_Vector* vectors, indexType rows, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        appendColumn(values, NV_DATA_S(vectors[k]), rows);
    }
}
//=============================================================================
void
appendEvent(std::vector<double>& xe, std::vector<double>& ye, std::vector<double>& ie,
    const double* y, indexType n, double t, int eventIndex)
{
    xe.push_back(t);
    appendColumn(ye, y, n);
    ie.push_back(static_cast<double>(eventIndex + 1));
}
//=============================================================================
std::vector<double>
solverOutputTimes(const NativeProblem& problem, const std::vector<double>& tspan)
{
    if (tspan.size() != 2) {
        return tspan;
    }
    double refineValue = 16;
    ArrayOf refine = getOptionalField(problem.options, "Refine");
    if (!refine.isEmpty()) {
        refineValue = std::max(refineValue, refine.asDoubleScalar());
    }
    const indexType count = static_cast<indexType>(std::max(2.0, refineValue + 1));
    std::vector<double> times(static_cast<size_t>(count));
    const double t0 = tspan.front();
    const double tfinal = tspan.back();
    for (indexType k = 0; k < count; ++k) {
        times[static_cast<size_t>(k)]
            = t0 + (tfinal - t0) * static_cast<double>(k) / static_cast<double>(count - 1);
    }
    return times;
}
//=============================================================================
std::string
lowerSundialsOption(
    const NativeProblem& problem, const std::string& fieldName, const std::string& defaultValue)
{
    ArrayOf value = getOptionalField(problem.options, fieldName);
    if (value.isEmpty()) {
        return defaultValue;
    }
    std::string text = value.asUtf8String();
    std::transform(text.begin(), text.end(), text.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return text;
}
//=============================================================================
std::string
linearSolverNameForProblem(const NativeProblem& problem)
{
    const std::string requested = lowerSundialsOption(problem, "LinearSolver", "auto");
    if (requested != "auto") {
        return requested;
    }
#ifdef NELSON_WITH_SUNDIALS_SPGMR
    if (hasSparseJacobianPattern(problem)) {
        return "spgmr";
    }
#endif
    return "dense";
}
//=============================================================================
bool
useIterativeLinearSolver(const std::string& linearSolver)
{
    return linearSolver != "dense" && linearSolver != "klu";
}
//=============================================================================
bool
useSparseDirectLinearSolver(const std::string& linearSolver)
{
    return linearSolver == "klu";
}
//=============================================================================
bool
hasDiagonalJacobianPattern(const NativeProblem& problem)
{
    if (!hasSparseJacobianPattern(problem)) {
        return false;
    }
    const JacobianBandwidth bandwidth = patternBandwidth(problem);
    return bandwidth.lower == 0 && bandwidth.upper == 0;
}
//=============================================================================
bool
hasNarrowBandedJacobianPattern(const NativeProblem& problem)
{
    if (!hasSparseJacobianPattern(problem)) {
        return false;
    }
    const JacobianBandwidth bandwidth = patternBandwidth(problem);
    return bandwidth.lower + bandwidth.upper <= 2;
}
//=============================================================================
std::string
autoPreconditionerNameForProblem(const NativeProblem& problem)
{
    if (hasDiagonalJacobianPattern(problem)) {
        return "jacobi";
    }
    if (hasNarrowBandedJacobianPattern(problem) && !densePreconditionerWouldBeLarge(problem.n)) {
        return "banded";
    }
    if (hasSparseJacobianPattern(problem)) {
        return "ilu0";
    }
    return "none";
}
//=============================================================================
std::string
preconditionerNameForProblem(const NativeProblem& problem, const std::string& linearSolver)
{
    const std::string requested = lowerSundialsOption(problem, "Preconditioner", "auto");
    if (linearSolver == "dense" || useSparseDirectLinearSolver(linearSolver)) {
        if (requested != "auto" && requested != "none") {
            Error(L"Nelson:ode_solvers:preconditionerRequiresIterativeSolver",
                L"SUNDIALS preconditioner requires an iterative linear solver.");
        }
        return "none";
    }
    if (requested == "auto") {
        return autoPreconditionerNameForProblem(problem);
    }
    return requested;
}
//=============================================================================
struct SundialsLinearStats
{
    long int linearSetupCount { 0 };
    long int linearIterations { 0 };
    long int linearConvergenceFailures { 0 };
    long int preconditionerSetupCount { 0 };
    long int preconditionerSolveCount { 0 };
};
//=============================================================================
ArrayOf
buildStats(long int nsteps, long int nfevals, double tfinal, const std::string& linearSolver,
    const std::string& preconditioner, const std::string& sensitivityMethod,
    const SundialsLinearStats& linearStats = {}, long int nfailed = 0)
{
    stringVector names { "nsteps", "nfailed", "nfevals", "tfinal", "linearSolver", "preconditioner",
        "sensitivityMethod", "linearSetupCount", "linearIterations", "linearConvergenceFailures",
        "preconditionerSetupCount", "preconditionerSolveCount" };
    ArrayOfVector values;
    values << ArrayOf::doubleScalar(static_cast<double>(nsteps));
    values << ArrayOf::doubleScalar(static_cast<double>(nfailed));
    values << ArrayOf::doubleScalar(static_cast<double>(nfevals));
    values << ArrayOf::doubleScalar(tfinal);
    values << ArrayOf::characterArray(linearSolver);
    values << ArrayOf::characterArray(preconditioner);
    values << ArrayOf::characterArray(sensitivityMethod);
    values << ArrayOf::doubleScalar(static_cast<double>(linearStats.linearSetupCount));
    values << ArrayOf::doubleScalar(static_cast<double>(linearStats.linearIterations));
    values << ArrayOf::doubleScalar(static_cast<double>(linearStats.linearConvergenceFailures));
    values << ArrayOf::doubleScalar(static_cast<double>(linearStats.preconditionerSetupCount));
    values << ArrayOf::doubleScalar(static_cast<double>(linearStats.preconditionerSolveCount));
    return ArrayOf::structArray(names, values);
}
//=============================================================================
ArrayOf
withSensitivityBackend(ArrayOf stats, const std::string& backend)
{
    stringVector names;
    ArrayOfVector values;
    for (const auto& name : stats.fieldNames()) {
        names.push_back(name);
        values << stats.field(name);
    }
    names.push_back("sensitivityBackend");
    values << ArrayOf::characterArray(backend);
    return ArrayOf::structArray(names, values);
}
//=============================================================================
ArrayOf
buildSolution(const NativeProblem& problem, const std::vector<double>& x,
    const std::vector<double>& y, const std::vector<double>& slopes, const std::vector<double>& xe,
    const std::vector<double>& ye, const std::vector<double>& ie, const ArrayOf& stats,
    const std::vector<double>& sensitivity = {}, const std::vector<double>& adjointGradient = {})
{
    stringVector extNames { "slopes", "odefun", "options", "yp0" };
    ArrayOfVector extValues;
    extValues << doubleArray(problem.n, static_cast<indexType>(x.size()), slopes);
    extValues << problem.odefun;
    extValues << problem.options;
    extValues << problem.yp0;
    ArrayOf extdata = ArrayOf::structArray(extNames, extValues);
    stringVector idataNames { "f3d", "idxNonNegative" };
    ArrayOfVector idataValues;
    idataValues << ArrayOf::empty();
    idataValues << getOptionalField(problem.options, "NonNegative");
    ArrayOf idata = ArrayOf::structArray(idataNames, idataValues);
    stringVector names { "solver", "extdata", "x", "y", "stats", "idata" };
    ArrayOfVector values;
    values << ArrayOf::characterArray(problem.solver);
    values << extdata;
    values << doubleArray(1, static_cast<indexType>(x.size()), x);
    values << doubleArray(problem.n, static_cast<indexType>(x.size()), y);
    values << stats;
    values << idata;
    if (!sensitivity.empty()) {
        names.push_back("sensitivity");
        values << doubleTensor(
            problem.n, problem.sensitivityCount, static_cast<indexType>(x.size()), sensitivity);
    }
    if (!adjointGradient.empty()) {
        names.push_back("adjointGradient");
        values << doubleArray(1, static_cast<indexType>(adjointGradient.size()), adjointGradient);
    }
    if (!xe.empty()) {
        names.push_back("xe");
        values << doubleArray(1, static_cast<indexType>(xe.size()), xe);
        names.push_back("ye");
        values << doubleArray(problem.n, static_cast<indexType>(xe.size()), ye);
        names.push_back("ie");
        values << doubleArray(1, static_cast<indexType>(ie.size()), ie);
    }
    return ArrayOf::structArray(names, values);
}
//=============================================================================
void
initializeEvents(NativeProblem& problem, double t0, const double* y0, const double* yp0)
{
    if (problem.events.isEmpty()) {
        return;
    }
    std::vector<double> values;
    evaluateEvents(
        &problem, t0, y0, yp0, values, &problem.eventData.terminal, &problem.eventData.direction);
    problem.eventData.count = static_cast<int>(values.size());
}
//=============================================================================
void
validateSensitivityParameterIndices(const NativeProblem& problem)
{
    // The finite-difference sensitivity callbacks index sensitivityParameters
    // directly with these values; an out-of-range entry would read or write
    // outside the vector.
    for (int parameterIndex : problem.sensitivityParameterList) {
        if (parameterIndex < 0
            || static_cast<size_t>(parameterIndex) >= problem.sensitivityParameters.size()) {
            Error(L"Nelson:ode_solvers:sensitivityIndexOutOfRange",
                L"Sensitivity parameter indices are out of range.");
        }
    }
}
//=============================================================================
void
initializeSensitivityProblem(NativeProblem& problem)
{
    ArrayOf sensitivity = getOptionalField(problem.options, FIELD_SENSITIVITY);
    if (sensitivity.isEmpty()) {
        return;
    }
    if (!sensitivity.isStruct()) {
        Error(L"Nelson:ode_solvers:sensitivityOptionsNotStruct",
            L"Sensitivity options must be a structure.");
    }
    ArrayOf method = getOptionalField(sensitivity, "Method");
    if (!method.isEmpty() && method.asWideString() == L"adjoint") {
        problem.useAdjointSensitivity = true;
        problem.sensitivityParameters = realDoubleVector(sensitivity.field("Parameters"),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityParametersRealDouble",
                L"Sensitivity parameters must be real double."));
        problem.sensitivityParameterList = integerVector(sensitivity.field("ParameterIndices"),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityParameterIndicesReal",
                L"Sensitivity parameter indices must be real double."));
        problem.sensitivityCount = static_cast<indexType>(problem.sensitivityParameterList.size());
        problem.adjointObjective = getOptionalField(sensitivity, "ObjectiveFcn");
        problem.adjointQuadrature = getOptionalField(sensitivity, "QuadratureFcn");
        ArrayOf objectiveTime = getOptionalField(sensitivity, "ObjectiveTime");
        if (!objectiveTime.isEmpty()) {
            problem.adjointObjectiveTime = objectiveTime.asDoubleScalar();
        }
        ArrayOf relTol = getOptionalField(sensitivity, "RelativeTolerance");
        if (!relTol.isEmpty()) {
            problem.adjointRelativeTolerance = relTol.asDoubleScalar();
        }
        ArrayOf absTol = getOptionalField(sensitivity, "AbsoluteTolerance");
        if (!absTol.isEmpty()) {
            problem.adjointAbsoluteTolerance = absTol.asDoubleScalar();
        }
        if (problem.sensitivityCount == 0) {
            Error(L"Nelson:ode_solvers:sensitivityOptionsSizeMismatch",
                L"Sensitivity options have incompatible size.");
        }
        validateSensitivityParameterIndices(problem);
        return;
    }
    problem.useNativeSensitivity = true;
    problem.sensitivityParameters = realDoubleVector(sensitivity.field("Parameters"),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityParametersRealDouble",
            L"Sensitivity parameters must be real double."));
    problem.sensitivityScales = realDoubleVector(sensitivity.field("ParameterScales"),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityParameterScalesReal",
            L"Sensitivity parameter scales must be real double."));
    problem.sensitivityParameterList = integerVector(sensitivity.field("ParameterIndices"),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityParameterIndicesReal",
            L"Sensitivity parameter indices must be real double."));
    problem.sensitivityInitialValue = realDoubleVector(sensitivity.field("InitialValue"),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityInitialRealDouble",
            L"Sensitivity initial value must be real double."));
    ArrayOf initialSlope = getOptionalField(sensitivity, "InitialSlope");
    if (!initialSlope.isEmpty()) {
        problem.sensitivityInitialSlope = realDoubleVector(initialSlope,
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:sensitivityInitialSlopeReal",
                L"Sensitivity initial slope must be real double."));
    }
    problem.sensitivityCount = static_cast<indexType>(problem.sensitivityParameterList.size());
    if (problem.sensitivityCount == 0
        || problem.sensitivityScales.size() != static_cast<size_t>(problem.sensitivityCount)
        || problem.sensitivityInitialValue.size()
            != static_cast<size_t>(problem.n * problem.sensitivityCount)) {
        Error(L"Nelson:ode_solvers:sensitivityOptionsSizeMismatch",
            L"Sensitivity options have incompatible size.");
    }
    if (!problem.sensitivityInitialSlope.empty()
        && problem.sensitivityInitialSlope.size()
            != static_cast<size_t>(problem.n * problem.sensitivityCount)) {
        Error(L"Nelson:ode_solvers:sensitivityInitialSlopeSizeMismatch",
            L"Sensitivity initial slope has incompatible size.");
    }
    validateSensitivityParameterIndices(problem);
}
//=============================================================================
