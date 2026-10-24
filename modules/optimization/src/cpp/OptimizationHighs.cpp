//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "OptimizationHighs.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <limits>
#include <numeric>
#include <string>
#include <vector>
//=============================================================================
#ifdef WITH_HIGHS
#include "highs/interfaces/highs_c_api.h"
#else
using HighsInt = int;
static constexpr HighsInt kHighsModelStatusOptimal = 7;
static constexpr HighsInt kHighsModelStatusInfeasible = 8;
static constexpr HighsInt kHighsModelStatusUnboundedOrInfeasible = 9;
static constexpr HighsInt kHighsModelStatusUnbounded = 10;
static constexpr HighsInt kHighsModelStatusObjectiveBound = 11;
static constexpr HighsInt kHighsModelStatusObjectiveTarget = 12;
static constexpr HighsInt kHighsModelStatusTimeLimit = 13;
static constexpr HighsInt kHighsModelStatusIterationLimit = 14;
static constexpr HighsInt kHighsModelStatusSolutionLimit = 16;
static constexpr HighsInt kHighsModelStatusInterrupt = 17;
#endif
//=============================================================================
namespace Nelson::OptimizationGateway {
//=============================================================================
using SparseDouble = Eigen::SparseMatrix<double, 0, signedIndexType>;
//=============================================================================
struct Options
{
    std::string display = "final";
    double maxTime = std::numeric_limits<double>::infinity();
    double maxNodes = std::numeric_limits<double>::infinity();
    double maxFeasiblePoints = std::numeric_limits<double>::infinity();
    double maxIterations = std::numeric_limits<double>::infinity();
    double lpMaxIterations = std::numeric_limits<double>::infinity();
    double absoluteGapTolerance = 1e-6;
    double relativeGapTolerance = 1e-4;
    double constraintTolerance = 1e-6;
    double integerTolerance = 1e-6;
    double lpOptimalityTolerance = 1e-7;
    double objectiveLimit = std::numeric_limits<double>::infinity();
    std::string lpPreprocess = "basic";
    std::string rootLPAlgorithm = "dual-simplex";
    std::string heuristics = "basic";
    std::string cutGeneration = "basic";
};
//=============================================================================
struct Problem
{
    std::vector<double> f;
    std::vector<int> intcon;
    std::vector<double> a;
    std::vector<double> b;
    std::vector<double> aeq;
    std::vector<double> beq;
    std::vector<double> lb;
    std::vector<double> ub;
    indexType aRows = 0;
    indexType aeqRows = 0;
    indexType n = 0;
    Options options;
};
//=============================================================================
struct CscMatrix
{
    std::vector<HighsInt> start;
    std::vector<HighsInt> index;
    std::vector<double> value;
};
//=============================================================================
static bool
hasField(const ArrayOf& value, const std::string& field)
{
    return (value.isStruct() || value.isClassType()) && value.isField(field);
}
//=============================================================================
static ArrayOf
fieldOrEmpty(const ArrayOf& value, const std::string& field)
{
    if (hasField(value, field)) {
        return value.field(field);
    }
    return ArrayOf::empty();
}
//=============================================================================
static double
fieldDouble(const ArrayOf& value, const std::string& field, double defaultValue)
{
    if (!hasField(value, field)) {
        return defaultValue;
    }
    ArrayOf data = value.field(field);
    if (data.isEmpty()) {
        return defaultValue;
    }
    return data.asDoubleScalar(true);
}
//=============================================================================
static std::string
lowerString(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return value;
}
//=============================================================================
static std::string
fieldString(const ArrayOf& value, const std::string& field, const std::string& defaultValue)
{
    if (!hasField(value, field)) {
        return defaultValue;
    }
    ArrayOf data = value.field(field);
    if (data.isEmpty()) {
        return defaultValue;
    }
    return wstring_to_utf8(data.asWideString());
}
//=============================================================================
static bool
isOneOf(const std::string& value, const std::vector<std::string>& choices)
{
    return std::find(choices.begin(), choices.end(), value) != choices.end();
}
//=============================================================================
static void
validateChoice(
    const std::string& value, const std::vector<std::string>& choices, const std::wstring& name)
{
    if (!isOneOf(value, choices)) {
        Error(L"Nelson:optim:invalidOptionValue", L"Invalid value for \"%s\".", name);
    }
}
//=============================================================================
static void
validateNonnegative(double value, const std::wstring& name)
{
    if (std::isfinite(value) && value < 0.) {
        Error(L"Nelson:validators:mustBeNonnegative", L"%s must be nonnegative.", name);
    }
}
//=============================================================================
static Options
parseOptions(const ArrayOf& value, const std::string& solver)
{
    Options opts;
    if (solver == "linprog") {
        opts.display = "final";
    }
    if (value.isEmpty()) {
        return opts;
    }
    ArrayOf source = value;
    if (hasField(value, "Store")) {
        source = value.field("Store");
    }
    opts.display = lowerString(fieldString(source, "Display", opts.display));
    opts.maxTime = fieldDouble(source, "MaxTime", opts.maxTime);
    opts.maxNodes = fieldDouble(source, "MaxNodes", opts.maxNodes);
    opts.maxFeasiblePoints = fieldDouble(source, "MaxFeasiblePoints", opts.maxFeasiblePoints);
    opts.maxFeasiblePoints = fieldDouble(source, "MaxNumFeasPoints", opts.maxFeasiblePoints);
    opts.maxIterations = fieldDouble(source, "MaxIterations", opts.maxIterations);
    opts.maxIterations = fieldDouble(source, "MaxIter", opts.maxIterations);
    opts.lpMaxIterations = fieldDouble(source, "LPMaxIterations", opts.lpMaxIterations);
    opts.absoluteGapTolerance
        = fieldDouble(source, "AbsoluteGapTolerance", opts.absoluteGapTolerance);
    opts.relativeGapTolerance
        = fieldDouble(source, "RelativeGapTolerance", opts.relativeGapTolerance);
    opts.relativeGapTolerance = fieldDouble(source, "TolGapRel", opts.relativeGapTolerance);
    opts.absoluteGapTolerance = fieldDouble(source, "TolGapAbs", opts.absoluteGapTolerance);
    opts.constraintTolerance = fieldDouble(source, "ConstraintTolerance", opts.constraintTolerance);
    opts.integerTolerance = fieldDouble(source, "IntegerTolerance", opts.integerTolerance);
    opts.integerTolerance = fieldDouble(source, "TolInteger", opts.integerTolerance);
    opts.lpOptimalityTolerance
        = fieldDouble(source, "LPOptimalityTolerance", opts.lpOptimalityTolerance);
    opts.objectiveLimit = fieldDouble(source, "ObjectiveLimit", opts.objectiveLimit);
    opts.lpPreprocess = lowerString(fieldString(source, "LPPreprocess", opts.lpPreprocess));
    opts.rootLPAlgorithm
        = lowerString(fieldString(source, "RootLPAlgorithm", opts.rootLPAlgorithm));
    opts.heuristics = lowerString(fieldString(source, "Heuristics", opts.heuristics));
    opts.cutGeneration = lowerString(fieldString(source, "CutGeneration", opts.cutGeneration));
    validateChoice(opts.display,
        { "off", "none", "final", "final-detailed", "notify", "notify-detailed", "iter",
            "iter-detailed" },
        L"Display");
    validateChoice(opts.lpPreprocess, { "none", "off", "basic", "on" }, L"LPPreprocess");
    validateChoice(opts.rootLPAlgorithm, { "dual-simplex", "primal-simplex" }, L"RootLPAlgorithm");
    validateChoice(opts.heuristics, { "none", "off", "basic", "advanced" }, L"Heuristics");
    validateChoice(opts.cutGeneration, { "none", "off", "basic", "advanced" }, L"CutGeneration");
    validateNonnegative(opts.maxTime, L"MaxTime");
    validateNonnegative(opts.maxNodes, L"MaxNodes");
    validateNonnegative(opts.maxFeasiblePoints, L"MaxFeasiblePoints");
    validateNonnegative(opts.maxIterations, L"MaxIterations");
    validateNonnegative(opts.lpMaxIterations, L"LPMaxIterations");
    validateNonnegative(opts.absoluteGapTolerance, L"AbsoluteGapTolerance");
    validateNonnegative(opts.relativeGapTolerance, L"RelativeGapTolerance");
    validateNonnegative(opts.constraintTolerance, L"ConstraintTolerance");
    validateNonnegative(opts.integerTolerance, L"IntegerTolerance");
    validateNonnegative(opts.lpOptimalityTolerance, L"LPOptimalityTolerance");
    return opts;
}
//=============================================================================
static std::vector<double>
asColumnVector(const ArrayOf& value, const std::string& name, int64 expectedLength = -1,
    double defaultValue = 0.)
{
    if (value.isEmpty()) {
        if (expectedLength < 0) {
            return {};
        }
        return std::vector<double>(static_cast<size_t>(expectedLength), defaultValue);
    }
    std::vector<double> data = value.asDoubleVector();
    if (expectedLength >= 0 && static_cast<int64>(data.size()) != expectedLength) {
        Error(L"Nelson:optim:invalidVectorLength", L"Invalid vector length for %s.",
            utf8_to_wstring(name));
    }
    return data;
}
//=============================================================================
static std::vector<int>
asIndexVector(const ArrayOf& value, indexType n)
{
    std::vector<double> raw = value.asDoubleVector();
    std::vector<int> indices;
    indices.reserve(raw.size());
    for (double item : raw) {
        double rounded = std::round(item);
        if (std::abs(item - rounded) > std::numeric_limits<double>::epsilon() || rounded < 1
            || rounded > static_cast<double>(n)) {
            Error(L"Nelson:optim:invalidIntconIndices",
                L"Integer constraint indices must be positive integer values.");
        }
        indices.push_back(static_cast<int>(rounded) - 1);
    }
    std::sort(indices.begin(), indices.end());
    indices.erase(std::unique(indices.begin(), indices.end()), indices.end());
    return indices;
}
//=============================================================================
static std::vector<double>
denseMatrix(const ArrayOf& value, indexType cols, indexType& rows, const std::string& name)
{
    if (value.isEmpty()) {
        rows = 0;
        return {};
    }
    if (value.columns() != cols) {
        Error(L"Nelson:optim:invalidColumns", L"Invalid number of columns for %s.",
            utf8_to_wstring(name));
    }
    rows = value.rows();
    if (value.isSparse()) {
        const auto* sparse = static_cast<const SparseDouble*>(value.sparseDataPointer());
        std::vector<double> data(static_cast<size_t>(rows * cols), 0.);
        for (int col = 0; col < sparse->outerSize(); ++col) {
            for (SparseDouble::InnerIterator it(*sparse, col); it; ++it) {
                data[static_cast<size_t>(it.row() + it.col() * rows)] = it.value();
            }
        }
        return data;
    }
    return value.asDoubleVector();
}
//=============================================================================
static void
validateRows(indexType rows, const std::vector<double>& rhs, const std::string& name)
{
    if (static_cast<indexType>(rhs.size()) != rows) {
        Error(L"Nelson:optim:invalidVectorLength", L"Invalid vector length for %s.",
            utf8_to_wstring(name));
    }
}
//=============================================================================
static Problem
parseProblemStruct(const ArrayOf& arg, bool mixedInteger)
{
    if (!arg.isStruct() && !arg.isClassType()) {
        Error(L"Nelson:optim:problemStructExpected", L"Problem structure expected.");
    }
    Problem problem;
    problem.f = asColumnVector(arg.field("f"), "f");
    problem.n = static_cast<indexType>(problem.f.size());
    if (mixedInteger) {
        problem.intcon = asIndexVector(arg.field("intcon"), problem.n);
    }
    ArrayOf a = fieldOrEmpty(arg, "Aineq");
    if (a.isEmpty()) {
        a = fieldOrEmpty(arg, "A");
    }
    ArrayOf b = fieldOrEmpty(arg, "bineq");
    if (b.isEmpty()) {
        b = fieldOrEmpty(arg, "b");
    }
    problem.a = denseMatrix(a, problem.n, problem.aRows, "A");
    problem.b = asColumnVector(b, "b", problem.aRows);
    validateRows(problem.aRows, problem.b, "b");
    problem.aeq = denseMatrix(fieldOrEmpty(arg, "Aeq"), problem.n, problem.aeqRows, "Aeq");
    problem.beq = asColumnVector(fieldOrEmpty(arg, "beq"), "beq", problem.aeqRows);
    validateRows(problem.aeqRows, problem.beq, "beq");
    problem.lb = asColumnVector(
        fieldOrEmpty(arg, "lb"), "lb", problem.n, -std::numeric_limits<double>::infinity());
    problem.ub = asColumnVector(
        fieldOrEmpty(arg, "ub"), "ub", problem.n, std::numeric_limits<double>::infinity());
    problem.options
        = parseOptions(fieldOrEmpty(arg, "options"), mixedInteger ? "intlinprog" : "linprog");
    return problem;
}
//=============================================================================
static Problem
parseDirectProblem(const ArrayOfVector& argIn, bool mixedInteger)
{
    Problem problem;
    problem.f = asColumnVector(argIn[0], "f");
    problem.n = static_cast<indexType>(problem.f.size());
    size_t offset = 1;
    if (mixedInteger) {
        problem.intcon = asIndexVector(argIn[1], problem.n);
        offset = 2;
    }
    ArrayOf A = argIn.size() > offset ? argIn[offset] : ArrayOf::empty();
    ArrayOf b = argIn.size() > offset + 1 ? argIn[offset + 1] : ArrayOf::empty();
    ArrayOf Aeq = argIn.size() > offset + 2 ? argIn[offset + 2] : ArrayOf::empty();
    ArrayOf beq = argIn.size() > offset + 3 ? argIn[offset + 3] : ArrayOf::empty();
    ArrayOf lb = argIn.size() > offset + 4 ? argIn[offset + 4] : ArrayOf::empty();
    ArrayOf ub = argIn.size() > offset + 5 ? argIn[offset + 5] : ArrayOf::empty();
    ArrayOf options = ArrayOf::empty();
    if (mixedInteger) {
        options = argIn.size() > offset + 7 ? argIn[offset + 7] : ArrayOf::empty();
    } else {
        options = argIn.size() > offset + 7
            ? argIn[offset + 7]
            : (argIn.size() > offset + 6 ? argIn[offset + 6] : ArrayOf::empty());
    }
    problem.a = denseMatrix(A, problem.n, problem.aRows, "A");
    problem.b = asColumnVector(b, "b", problem.aRows);
    validateRows(problem.aRows, problem.b, "b");
    problem.aeq = denseMatrix(Aeq, problem.n, problem.aeqRows, "Aeq");
    problem.beq = asColumnVector(beq, "beq", problem.aeqRows);
    validateRows(problem.aeqRows, problem.beq, "beq");
    problem.lb = asColumnVector(lb, "lb", problem.n, -std::numeric_limits<double>::infinity());
    problem.ub = asColumnVector(ub, "ub", problem.n, std::numeric_limits<double>::infinity());
    problem.options = parseOptions(options, mixedInteger ? "intlinprog" : "linprog");
    return problem;
}
//=============================================================================
static CscMatrix
makeCsc(const Problem& problem)
{
    CscMatrix csc;
    csc.start.resize(static_cast<size_t>(problem.n + 1), 0);
    for (indexType col = 0; col < problem.n; ++col) {
        csc.start[static_cast<size_t>(col)] = static_cast<HighsInt>(csc.index.size());
        for (indexType row = 0; row < problem.aRows; ++row) {
            double value = problem.a[static_cast<size_t>(row + col * problem.aRows)];
            if (value != 0.) {
                csc.index.push_back(static_cast<HighsInt>(row));
                csc.value.push_back(value);
            }
        }
        for (indexType row = 0; row < problem.aeqRows; ++row) {
            double value = problem.aeq[static_cast<size_t>(row + col * problem.aeqRows)];
            if (value != 0.) {
                csc.index.push_back(static_cast<HighsInt>(problem.aRows + row));
                csc.value.push_back(value);
            }
        }
    }
    csc.start[static_cast<size_t>(problem.n)] = static_cast<HighsInt>(csc.index.size());
    return csc;
}
//=============================================================================
static std::vector<double>
rowLower(const Problem& problem)
{
    std::vector<double> lower(static_cast<size_t>(problem.aRows + problem.aeqRows),
        -std::numeric_limits<double>::infinity());
    for (indexType row = 0; row < problem.aeqRows; ++row) {
        lower[static_cast<size_t>(problem.aRows + row)] = problem.beq[static_cast<size_t>(row)];
    }
    return lower;
}
//=============================================================================
static std::vector<double>
rowUpper(const Problem& problem)
{
    std::vector<double> upper;
    upper.reserve(static_cast<size_t>(problem.aRows + problem.aeqRows));
    upper.insert(upper.end(), problem.b.begin(), problem.b.end());
    upper.insert(upper.end(), problem.beq.begin(), problem.beq.end());
    return upper;
}
//=============================================================================
static ArrayOf
columnVector(const std::vector<double>& data)
{
    ArrayOf out = ArrayOf::doubleMatrix2d(static_cast<indexType>(data.size()), 1);
    if (!data.empty()) {
        auto* ptr = static_cast<double*>(out.readWriteDataPointer());
        std::copy(data.begin(), data.end(), ptr);
    }
    return out;
}
//=============================================================================
static double
maxViolation(const Problem& problem, const std::vector<double>& x)
{
    if (x.empty()) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    double violation = 0.;
    for (indexType row = 0; row < problem.aRows; ++row) {
        double value = 0.;
        for (indexType col = 0; col < problem.n; ++col) {
            value += problem.a[static_cast<size_t>(row + col * problem.aRows)]
                * x[static_cast<size_t>(col)];
        }
        violation = std::max(violation, value - problem.b[static_cast<size_t>(row)]);
    }
    for (indexType row = 0; row < problem.aeqRows; ++row) {
        double value = 0.;
        for (indexType col = 0; col < problem.n; ++col) {
            value += problem.aeq[static_cast<size_t>(row + col * problem.aeqRows)]
                * x[static_cast<size_t>(col)];
        }
        violation = std::max(violation, std::abs(value - problem.beq[static_cast<size_t>(row)]));
    }
    for (indexType col = 0; col < problem.n; ++col) {
        violation = std::max(
            violation, problem.lb[static_cast<size_t>(col)] - x[static_cast<size_t>(col)]);
        violation = std::max(
            violation, x[static_cast<size_t>(col)] - problem.ub[static_cast<size_t>(col)]);
    }
    return violation;
}
//=============================================================================
static std::string
statusMessage(HighsInt modelStatus)
{
    switch (modelStatus) {
    case kHighsModelStatusOptimal:
        return "Optimal solution found.";
    case kHighsModelStatusInfeasible:
        return "Problem is infeasible.";
    case kHighsModelStatusUnbounded:
        return "Problem is unbounded.";
    case kHighsModelStatusObjectiveBound:
        return "Solver stopped because the objective limit was reached.";
    case kHighsModelStatusObjectiveTarget:
        return "Solver stopped because the objective target was reached.";
    case kHighsModelStatusTimeLimit:
        return "Solver stopped because the time limit was reached.";
    case kHighsModelStatusIterationLimit:
        return "Solver stopped because the iteration limit was reached.";
    case kHighsModelStatusSolutionLimit:
        return "Solver stopped because the feasible point limit was reached.";
    case kHighsModelStatusInterrupt:
        return "Solver stopped by interruption.";
    default:
        return "Solver stopped before an optimal solution was found.";
    }
}
//=============================================================================
static std::string
statusName(HighsInt modelStatus)
{
    switch (modelStatus) {
    case kHighsModelStatusOptimal:
        return "optimal";
    case kHighsModelStatusInfeasible:
        return "infeasible";
    case kHighsModelStatusUnboundedOrInfeasible:
        return "unbounded-or-infeasible";
    case kHighsModelStatusUnbounded:
        return "unbounded";
    case kHighsModelStatusObjectiveBound:
        return "objective-bound";
    case kHighsModelStatusObjectiveTarget:
        return "objective-target";
    case kHighsModelStatusTimeLimit:
        return "time-limit";
    case kHighsModelStatusIterationLimit:
        return "iteration-limit";
    case kHighsModelStatusSolutionLimit:
        return "solution-limit";
    case kHighsModelStatusInterrupt:
        return "interrupt";
    default:
        return "unknown";
    }
}
//=============================================================================
static bool
isLimitStatus(HighsInt modelStatus)
{
    return modelStatus == kHighsModelStatusObjectiveBound
        || modelStatus == kHighsModelStatusObjectiveTarget
        || modelStatus == kHighsModelStatusTimeLimit
        || modelStatus == kHighsModelStatusIterationLimit
        || modelStatus == kHighsModelStatusSolutionLimit
        || modelStatus == kHighsModelStatusInterrupt;
}
//=============================================================================
static bool
isTerminalWithoutPoint(HighsInt modelStatus)
{
    return modelStatus == kHighsModelStatusInfeasible
        || modelStatus == kHighsModelStatusUnboundedOrInfeasible
        || modelStatus == kHighsModelStatusUnbounded;
}
//=============================================================================
static double
objectiveValue(const Problem& problem, const std::vector<double>& x)
{
    if (x.empty()) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return std::inner_product(problem.f.begin(), problem.f.end(), x.begin(), 0.);
}
//=============================================================================
static void
validateProblem(const Problem& problem)
{
    for (indexType k = 0; k < problem.n; ++k) {
        double lower = problem.lb[static_cast<size_t>(k)];
        double upper = problem.ub[static_cast<size_t>(k)];
        if (lower > upper) {
            Error(L"Nelson:optimization:boundsInconsistent",
                L"Lower bounds must not exceed upper bounds.");
        }
        if (!std::isfinite(problem.f[static_cast<size_t>(k)])) {
            Error(L"Nelson:optim:objectiveNotFinite", L"Objective coefficients must be finite.");
        }
    }
}
//=============================================================================
static int
exitFlag(HighsInt modelStatus, bool hasSolution)
{
    switch (modelStatus) {
    case kHighsModelStatusOptimal:
        return 1;
    case kHighsModelStatusInfeasible:
    case kHighsModelStatusUnboundedOrInfeasible:
        return -2;
    case kHighsModelStatusUnbounded:
        return -3;
    case kHighsModelStatusObjectiveBound:
    case kHighsModelStatusObjectiveTarget:
    case kHighsModelStatusTimeLimit:
    case kHighsModelStatusIterationLimit:
    case kHighsModelStatusSolutionLimit:
    case kHighsModelStatusInterrupt:
        return hasSolution ? 2 : 0;
    default:
        return 0;
    }
}
//=============================================================================
static ArrayOf
makeOutput(const Problem& problem, const std::vector<double>& x, HighsInt modelStatus,
    const std::string& algorithm, double runtime, double relativeGap, double absoluteGap,
    double nodes, double numFeasible, double iterations, double firstOrderOpt,
    double primalSolutionStatus)
{
    stringVector names { "relativegap", "absolutegap", "numfeaspoints", "numnodes",
        "constrviolation", "algorithm", "message", "time", "iterations", "cgiterations",
        "firstorderopt", "status", "primalstatus" };
    ArrayOfVector values;
    values << ArrayOf::doubleScalar(relativeGap);
    values << ArrayOf::doubleScalar(absoluteGap);
    values << ArrayOf::doubleScalar(numFeasible);
    values << ArrayOf::doubleScalar(nodes);
    values << ArrayOf::doubleScalar(maxViolation(problem, x));
    values << ArrayOf::characterArray(algorithm);
    values << ArrayOf::characterArray(statusMessage(modelStatus));
    values << ArrayOf::doubleScalar(runtime);
    values << ArrayOf::doubleScalar(iterations);
    values << ArrayOf::doubleScalar(0.);
    values << ArrayOf::doubleScalar(firstOrderOpt);
    values << ArrayOf::characterArray(statusName(modelStatus));
    values << ArrayOf::doubleScalar(primalSolutionStatus);
    return ArrayOf::structArray(names, values);
}
//=============================================================================
static ArrayOf
emptyLambda()
{
    stringVector names { "lower", "upper", "ineqlin", "eqlin" };
    ArrayOfVector values;
    values << ArrayOf::empty();
    values << ArrayOf::empty();
    values << ArrayOf::empty();
    values << ArrayOf::empty();
    return ArrayOf::structArray(names, values);
}
//=============================================================================
static ArrayOf
linearLambda(const Problem& problem, const std::vector<double>& colDual,
    const std::vector<double>& rowDual, bool mixedInteger)
{
    if (mixedInteger || colDual.empty()) {
        return emptyLambda();
    }
    std::vector<double> lower(static_cast<size_t>(problem.n), 0.);
    std::vector<double> upper(static_cast<size_t>(problem.n), 0.);
    for (indexType k = 0; k < problem.n; ++k) {
        double value = colDual[static_cast<size_t>(k)];
        if (value > 0.) {
            lower[static_cast<size_t>(k)] = value;
        } else {
            upper[static_cast<size_t>(k)] = -value;
        }
    }
    std::vector<double> ineqlin(static_cast<size_t>(problem.aRows), 0.);
    for (indexType k = 0; k < problem.aRows; ++k) {
        ineqlin[static_cast<size_t>(k)] = -rowDual[static_cast<size_t>(k)];
    }
    std::vector<double> eqlin(static_cast<size_t>(problem.aeqRows), 0.);
    for (indexType k = 0; k < problem.aeqRows; ++k) {
        eqlin[static_cast<size_t>(k)] = rowDual[static_cast<size_t>(problem.aRows + k)];
    }

    stringVector names { "lower", "upper", "ineqlin", "eqlin" };
    ArrayOfVector values;
    values << columnVector(lower);
    values << columnVector(upper);
    values << columnVector(ineqlin);
    values << columnVector(eqlin);
    return ArrayOf::structArray(names, values);
}
//=============================================================================
#ifdef WITH_HIGHS
static void
setHighsOptions(void* highs, const Options& opts, bool mixedInteger)
{
    bool quiet = opts.display == "off" || opts.display == "none";
    Highs_setBoolOptionValue(highs, "output_flag", quiet ? 0 : 1);
    if (opts.display == "iter") {
        Highs_setIntOptionValue(highs, "log_dev_level", 1);
    }
    if (std::isfinite(opts.maxTime)) {
        Highs_setDoubleOptionValue(highs, "time_limit", opts.maxTime);
    }
    double simplexLimit = opts.lpMaxIterations;
    if (!std::isfinite(simplexLimit)) {
        simplexLimit = opts.maxIterations;
    }
    if (std::isfinite(simplexLimit)) {
        Highs_setIntOptionValue(
            highs, "simplex_iteration_limit", static_cast<HighsInt>(std::max(0., simplexLimit)));
    }
    Highs_setDoubleOptionValue(highs, "primal_feasibility_tolerance", opts.constraintTolerance);
    Highs_setDoubleOptionValue(highs, "dual_feasibility_tolerance", opts.constraintTolerance);
    Highs_setDoubleOptionValue(highs, "optimality_tolerance", opts.lpOptimalityTolerance);
    if (std::isfinite(opts.objectiveLimit)) {
        Highs_setDoubleOptionValue(highs, "objective_bound", opts.objectiveLimit);
        Highs_setDoubleOptionValue(highs, "objective_target", opts.objectiveLimit);
    }
    if (mixedInteger) {
        if (std::isfinite(opts.maxNodes)) {
            Highs_setIntOptionValue(
                highs, "mip_max_nodes", static_cast<HighsInt>(std::max(0., opts.maxNodes)));
        }
        if (std::isfinite(opts.maxFeasiblePoints) && opts.maxFeasiblePoints >= 1.) {
            Highs_setIntOptionValue(
                highs, "mip_max_improving_sols", static_cast<HighsInt>(opts.maxFeasiblePoints));
        }
        Highs_setDoubleOptionValue(highs, "mip_abs_gap", opts.absoluteGapTolerance);
        Highs_setDoubleOptionValue(highs, "mip_rel_gap", opts.relativeGapTolerance);
        Highs_setDoubleOptionValue(highs, "mip_feasibility_tolerance", opts.integerTolerance);
        if (opts.heuristics == "none" || opts.heuristics == "off") {
            Highs_setDoubleOptionValue(highs, "mip_heuristic_effort", 0.);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_feasibility_jump", 0);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_rins", 0);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_rens", 0);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_root_reduced_cost", 0);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_zi_round", 0);
            Highs_setBoolOptionValue(highs, "mip_heuristic_run_shifting", 0);
        } else if (opts.heuristics == "advanced") {
            Highs_setDoubleOptionValue(highs, "mip_heuristic_effort", 0.2);
        } else if (opts.heuristics == "basic") {
            Highs_setDoubleOptionValue(highs, "mip_heuristic_effort", 0.05);
        }
        if (opts.cutGeneration == "none" || opts.cutGeneration == "off") {
            Highs_setBoolOptionValue(highs, "mip_allow_cut_separation_at_nodes", 0);
        }
    }
    if (opts.lpPreprocess == "none" || opts.lpPreprocess == "off") {
        Highs_setStringOptionValue(highs, "presolve", "off");
    } else if (opts.lpPreprocess == "basic" || opts.lpPreprocess == "on") {
        Highs_setStringOptionValue(highs, "presolve", "on");
    }
    if (opts.rootLPAlgorithm == "primal-simplex") {
        Highs_setIntOptionValue(highs, "simplex_strategy", 4);
    } else if (opts.rootLPAlgorithm == "dual-simplex") {
        Highs_setIntOptionValue(highs, "simplex_strategy", 1);
    }
}
//=============================================================================
static double
getIntInfo(void* highs, const char* name, double defaultValue)
{
    HighsInt value = 0;
    if (Highs_getIntInfoValue(highs, name, &value) != kHighsStatusOk) {
        return defaultValue;
    }
    return static_cast<double>(value);
}
//=============================================================================
static double
getDoubleInfo(void* highs, const char* name, double defaultValue)
{
    double value = defaultValue;
    if (Highs_getDoubleInfoValue(highs, name, &value) != kHighsStatusOk) {
        return defaultValue;
    }
    return value;
}
//=============================================================================
static double
getInt64Info(void* highs, const char* name, double defaultValue)
{
    int64_t value = 0;
    if (Highs_getInt64InfoValue(highs, name, &value) != kHighsStatusOk) {
        return defaultValue;
    }
    return static_cast<double>(value);
}
//=============================================================================
static ArrayOfVector
solveHighs(int nLhs, const Problem& problem, bool mixedInteger)
{
    validateProblem(problem);
    CscMatrix csc = makeCsc(problem);
    std::vector<double> lowerRows = rowLower(problem);
    std::vector<double> upperRows = rowUpper(problem);
    std::vector<HighsInt> integrality(static_cast<size_t>(problem.n), kHighsVarTypeContinuous);
    for (int idx : problem.intcon) {
        integrality[static_cast<size_t>(idx)] = kHighsVarTypeInteger;
    }

    void* highs = Highs_create();
    if (highs == nullptr) {
        Error(L"Nelson:optim:solverInitFailed", L"HiGHS solver could not be initialized.");
    }
    setHighsOptions(highs, problem.options, mixedInteger);
    HighsInt status = mixedInteger
        ? Highs_passMip(highs, static_cast<HighsInt>(problem.n),
              static_cast<HighsInt>(problem.aRows + problem.aeqRows),
              static_cast<HighsInt>(csc.value.size()), kHighsMatrixFormatColwise,
              kHighsObjSenseMinimize, 0., problem.f.data(), problem.lb.data(), problem.ub.data(),
              lowerRows.data(), upperRows.data(), csc.start.data(), csc.index.data(),
              csc.value.data(), integrality.data())
        : Highs_passLp(highs, static_cast<HighsInt>(problem.n),
              static_cast<HighsInt>(problem.aRows + problem.aeqRows),
              static_cast<HighsInt>(csc.value.size()), kHighsMatrixFormatColwise,
              kHighsObjSenseMinimize, 0., problem.f.data(), problem.lb.data(), problem.ub.data(),
              lowerRows.data(), upperRows.data(), csc.start.data(), csc.index.data(),
              csc.value.data());
    if (status != kHighsStatusOk) {
        Highs_destroy(highs);
        Error(L"Nelson:optim:invalidProblemData", L"Invalid optimization problem data.");
    }
    status = Highs_run(highs);
    HighsInt modelStatus = Highs_getModelStatus(highs);
    double primalSolutionStatus = getIntInfo(highs, "primal_solution_status", 0.);
    std::vector<double> x(static_cast<size_t>(problem.n), 0.);
    std::vector<double> colDual(static_cast<size_t>(problem.n), 0.);
    std::vector<double> rowValue(static_cast<size_t>(problem.aRows + problem.aeqRows), 0.);
    std::vector<double> rowDual(static_cast<size_t>(problem.aRows + problem.aeqRows), 0.);
    Highs_getSolution(highs, x.data(), colDual.data(), rowValue.data(), rowDual.data());
    double backendObjective = getDoubleInfo(
        highs, "objective_function_value", std::numeric_limits<double>::quiet_NaN());
    bool terminalWithoutPoint = isTerminalWithoutPoint(modelStatus);
    double rawViolation = terminalWithoutPoint ? std::numeric_limits<double>::quiet_NaN()
                                               : maxViolation(problem, x);
    bool hasFeasiblePoint = !terminalWithoutPoint && std::isfinite(backendObjective)
        && rawViolation <= std::max(1e-7, problem.options.constraintTolerance);
    bool hasSolution = modelStatus == kHighsModelStatusOptimal
        || (isLimitStatus(modelStatus) && !terminalWithoutPoint
            && (primalSolutionStatus == 2. || hasFeasiblePoint));
    if (!hasSolution) {
        x.clear();
    }
    double fval = hasSolution
        ? (std::isfinite(backendObjective) ? backendObjective : objectiveValue(problem, x))
        : std::numeric_limits<double>::quiet_NaN();
    double runtime = Highs_getRunTime(highs);
    double relativeGap = mixedInteger ? getDoubleInfo(highs, "mip_gap", 0.) : 0.;
    double dualBound = mixedInteger ? getDoubleInfo(highs, "mip_dual_bound", fval) : fval;
    double absoluteGap = mixedInteger ? std::abs(fval - dualBound) : 0.;
    double nodes = mixedInteger ? getInt64Info(highs, "mip_node_count", 0.) : 0.;
    double simplexIterations = getIntInfo(highs, "simplex_iteration_count", 0.);
    double ipmIterations = getIntInfo(highs, "ipm_iteration_count", 0.);
    double iterations = mixedInteger
        ? getInt64Info(highs, "mip_total_lp_iterations", simplexIterations)
        : simplexIterations + ipmIterations;
    double feasible = hasSolution ? 1. : 0.;
    double firstOrderOpt = hasSolution
        ? (std::isfinite(rawViolation) ? rawViolation : maxViolation(problem, x))
        : std::numeric_limits<double>::quiet_NaN();

    ArrayOfVector retval;
    retval << (hasSolution ? columnVector(x) : ArrayOf::empty());
    if (nLhs > 1) {
        retval << (hasSolution ? ArrayOf::doubleScalar(fval) : ArrayOf::empty());
    }
    if (nLhs > 2) {
        retval << ArrayOf::doubleScalar(exitFlag(modelStatus, hasSolution));
    }
    if (nLhs > 3) {
        retval << makeOutput(problem, x, modelStatus, "highs", runtime, relativeGap, absoluteGap,
            nodes, feasible, iterations, firstOrderOpt, primalSolutionStatus);
    }
    if (nLhs > 4) {
        retval << linearLambda(problem, colDual, rowDual, mixedInteger);
    }
    Highs_destroy(highs);
    if (status != kHighsStatusOk && retval.empty()) {
        Error(L"Nelson:optim:solverFailed", L"HiGHS solver failed.");
    }
    return retval;
}
#else
static ArrayOfVector
solveHighs(int, const Problem&, bool)
{
    Error(L"Nelson:optim:backendNotAvailable", L"HiGHS backend is not available in this build.");
    return {};
}
#endif
//=============================================================================
ArrayOfVector
intlinprogBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() == 1 && (argIn[0].isStruct() || argIn[0].isClassType())) {
        return solveHighs(nLhs, parseProblemStruct(argIn[0], true), true);
    }
    if (argIn.size() < 4) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    if (argIn.size() > 10) {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return solveHighs(nLhs, parseDirectProblem(argIn, true), true);
}
//=============================================================================
ArrayOfVector
linprogBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() == 1 && (argIn[0].isStruct() || argIn[0].isClassType())) {
        return solveHighs(nLhs, parseProblemStruct(argIn[0], false), false);
    }
    if (argIn.empty()) {
        Error(L"Nelson:notEnoughInputs", L"Not enough input arguments.");
    }
    if (argIn.size() > 9) {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return solveHighs(nLhs, parseDirectProblem(argIn, false), false);
}
//=============================================================================
} // namespace Nelson::OptimizationGateway
//=============================================================================
