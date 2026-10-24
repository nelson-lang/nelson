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
jacobianDiagonal(NativeProblem* problem, double t, const double* y, const double* fy)
{
    ArrayOf jacobian = getOptionalField(problem->options, "Jacobian");
    if (!jacobian.isEmpty()) {
        if (jacobian.isFunctionHandle()) {
            ArrayOfVector out = callProblemFunction(
                problem->eval, jacobian, t, y, problem->n, currentParameters(problem), 1);
            if (out.empty()) {
                Error(
                    L"Nelson:ode_solvers:jacobianNoValue", L"Jacobian function returned no value.");
            }
            return matrixDiagonalValues(out[0], problem->n,
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianRealDouble",
                    L"Jacobian value must be real double."),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianSizeMismatch",
                    L"Jacobian matrix has incompatible size."));
        } else {
            return matrixDiagonalValues(jacobian, problem->n,
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianRealDouble",
                    L"Jacobian value must be real double."),
                Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianSizeMismatch",
                    L"Jacobian matrix has incompatible size."));
        }
    }
    std::vector<double> diagonal(static_cast<size_t>(problem->n));
    std::vector<double> trial(y, y + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        ArrayOfVector out = callProblemFunction(problem->eval, problem->odefun, t, trial.data(),
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
        rhs = applyMass(problem, t, trial.data(), rhs);
        diagonal[idx] = (rhs[idx] - fy[k]) / step;
        trial[idx] = saved;
    }
    return diagonal;
}
//=============================================================================
std::vector<double>
cvodesJacobianMatrix(NativeProblem* problem, double t, const double* y, const double* fy)
{
    const JacobianBandwidth bandwidth = patternBandwidth(*problem);
    ArrayOf jacobian = getOptionalField(problem->options, "Jacobian");
    if (!jacobian.isEmpty()) {
        ArrayOf value = jacobian;
        if (jacobian.isFunctionHandle()) {
            ArrayOfVector out = callProblemFunction(
                problem->eval, jacobian, t, y, problem->n, currentParameters(problem), 1);
            if (out.empty()) {
                Error(
                    L"Nelson:ode_solvers:jacobianNoValue", L"Jacobian function returned no value.");
            }
            value = out[0];
        }
        std::vector<double> matrix = matrixValues(value, problem->n,
            Nelson::resolveMessageTemplate(
                L"Nelson:ode_solvers:jacobianRealDouble", L"Jacobian value must be real double."),
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianSizeMismatch",
                L"Jacobian matrix has incompatible size."));
        zeroOutsideBand(matrix, problem->n, bandwidth);
        return matrix;
    }
    std::vector<double> matrix(static_cast<size_t>(problem->n * problem->n), 0);
    std::vector<double> trial(y, y + problem->n);
    for (indexType col = 0; col < problem->n; ++col) {
        const size_t idx = static_cast<size_t>(col);
        const double saved = trial[idx];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[idx] = saved + step;
        ArrayOfVector out = callProblemFunction(problem->eval, problem->odefun, t, trial.data(),
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
        rhs = applyMass(problem, t, trial.data(), rhs);
        for (indexType row = 0; row < problem->n; ++row) {
            const bool insideLower = row <= col || row - col <= bandwidth.lower;
            const bool insideUpper = col <= row || col - row <= bandwidth.upper;
            if (insideLower && insideUpper) {
                matrix[static_cast<size_t>(row + col * problem->n)] = (rhs[row] - fy[row]) / step;
            }
        }
        trial[idx] = saved;
    }
    return matrix;
}
//=============================================================================
void
setSafePivot(std::vector<double>& matrix, indexType n)
{
    for (indexType k = 0; k < n; ++k) {
        double& value = matrix[static_cast<size_t>(k + k * n)];
        double scale = 0;
        for (indexType col = 0; col < n; ++col) {
            scale = std::max(scale, std::abs(matrix[static_cast<size_t>(k + col * n)]));
        }
        const double threshold = std::max(std::numeric_limits<double>::epsilon(),
            std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, scale));
        if (std::abs(value) < threshold) {
            value = value < 0 ? -threshold : threshold;
        }
    }
}
//=============================================================================
std::vector<double>
cvodesPreconditionerMatrix(
    NativeProblem* problem, double t, const double* y, const double* fy, double gamma)
{
    if (densePreconditionerWouldBeLarge(problem->n)) {
        Error(L"Nelson:ode_solvers:preconditionerTooLarge",
            L"SUNDIALS preconditioner setup is too large for dense workspace.");
    }
    std::vector<double> jac = cvodesJacobianMatrix(problem, t, y, fy);
    std::vector<double> matrix(static_cast<size_t>(problem->n * problem->n), 0);
    for (indexType col = 0; col < problem->n; ++col) {
        for (indexType row = 0; row < problem->n; ++row) {
            const size_t idx = static_cast<size_t>(row + col * problem->n);
            matrix[idx] = -gamma * jac[idx];
        }
        matrix[static_cast<size_t>(col + col * problem->n)] += 1.0;
    }
    setSafePivot(matrix, problem->n);
    return matrix;
}
//=============================================================================
void
zeroOutsidePattern(
    std::vector<double>& matrix, const std::vector<unsigned char>& pattern, indexType n)
{
    for (indexType col = 0; col < n; ++col) {
        for (indexType row = 0; row < n; ++row) {
            const size_t idx = static_cast<size_t>(row + col * n);
            if (pattern[idx] == 0) {
                matrix[idx] = 0;
            }
        }
    }
}
//=============================================================================
double*
ilu0Entry(SparseIlu0Factor& factor, indexType row, indexType col)
{
    for (indexType k = factor.rowStart[static_cast<size_t>(row)];
        k < factor.rowStart[static_cast<size_t>(row + 1)]; ++k) {
        if (factor.columns[static_cast<size_t>(k)] == col) {
            return &factor.values[static_cast<size_t>(k)];
        }
    }
    return nullptr;
}
//=============================================================================
const double*
ilu0Entry(const SparseIlu0Factor& factor, indexType row, indexType col)
{
    for (indexType k = factor.rowStart[static_cast<size_t>(row)];
        k < factor.rowStart[static_cast<size_t>(row + 1)]; ++k) {
        if (factor.columns[static_cast<size_t>(k)] == col) {
            return &factor.values[static_cast<size_t>(k)];
        }
    }
    return nullptr;
}
//=============================================================================
void
setSafePivot(SparseIlu0Factor& factor)
{
    for (indexType k = 0; k < factor.n; ++k) {
        double* value = ilu0Entry(factor, k, k);
        if (value == nullptr) {
            Error(L"Nelson:ode_solvers:preconditionerPatternSizeMismatch",
                L"Preconditioner pattern has incompatible size.");
        }
        double scale = 0;
        for (indexType idx = factor.rowStart[static_cast<size_t>(k)];
            idx < factor.rowStart[static_cast<size_t>(k + 1)]; ++idx) {
            scale = std::max(scale, std::abs(factor.values[static_cast<size_t>(idx)]));
        }
        const double threshold = std::max(std::numeric_limits<double>::epsilon(),
            std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, scale));
        if (std::abs(*value) < threshold) {
            *value = *value < 0 ? -threshold : threshold;
        }
    }
}
//=============================================================================
SparseIlu0Factor
emptyIlu0Factor(const SparseJacobianPattern& pattern)
{
    SparseIlu0Factor factor;
    factor.n = pattern.n;
    factor.rowStart = pattern.rowStart;
    factor.columns = pattern.columns;
    factor.columnStart = pattern.columnStart;
    factor.rows = pattern.rows;
    factor.values.assign(pattern.columns.size(), 0);
    return factor;
}
//=============================================================================
void
buildIlu0ColumnIndex(SparseIlu0Factor& factor)
{
    std::vector<std::vector<indexType>> columnRows(static_cast<size_t>(factor.n));
    for (indexType row = 0; row < factor.n; ++row) {
        for (indexType idx = factor.rowStart[static_cast<size_t>(row)];
            idx < factor.rowStart[static_cast<size_t>(row + 1)]; ++idx) {
            const indexType col = factor.columns[static_cast<size_t>(idx)];
            columnRows[static_cast<size_t>(col)].push_back(row);
        }
    }
    factor.columnStart.assign(static_cast<size_t>(factor.n + 1), 0);
    factor.rows.clear();
    for (indexType col = 0; col < factor.n; ++col) {
        appendSortedUnique(factor.rows, columnRows[static_cast<size_t>(col)]);
        factor.columnStart[static_cast<size_t>(col + 1)]
            = static_cast<indexType>(factor.rows.size());
    }
}
//=============================================================================
void
factorIlu0(SparseIlu0Factor& factor)
{
    setSafePivot(factor);
    for (indexType k = 0; k < factor.n; ++k) {
        double* pivotEntry = ilu0Entry(factor, k, k);
        if (pivotEntry == nullptr) {
            Error(L"Nelson:ode_solvers:preconditionerPatternSizeMismatch",
                L"Preconditioner pattern has incompatible size.");
        }
        const double pivot = *pivotEntry;
        for (indexType rowIndex = factor.columnStart[static_cast<size_t>(k)];
            rowIndex < factor.columnStart[static_cast<size_t>(k + 1)]; ++rowIndex) {
            const indexType row = factor.rows[static_cast<size_t>(rowIndex)];
            if (row <= k) {
                continue;
            }
            double* rowCol = ilu0Entry(factor, row, k);
            if (rowCol == nullptr) {
                continue;
            }
            *rowCol /= pivot;
            const double multiplier = *rowCol;
            for (indexType idx = factor.rowStart[static_cast<size_t>(k)];
                idx < factor.rowStart[static_cast<size_t>(k + 1)]; ++idx) {
                const indexType col = factor.columns[static_cast<size_t>(idx)];
                if (col <= k) {
                    continue;
                }
                double* rowValue = ilu0Entry(factor, row, col);
                if (rowValue != nullptr) {
                    *rowValue -= multiplier * factor.values[static_cast<size_t>(idx)];
                }
            }
        }
    }
}
//=============================================================================
SparseIlu0Factor
makeIlu0Factor(const std::vector<double>& matrix, std::vector<unsigned char> pattern, indexType n)
{
    if (matrix.size() != static_cast<size_t>(n * n)
        || pattern.size() != static_cast<size_t>(n * n)) {
        Error(L"Nelson:ode_solvers:preconditionerSizeMismatch",
            L"Preconditioner has incompatible size.");
    }
    setPatternDiagonal(pattern, n);
    SparseIlu0Factor factor;
    factor.n = n;
    factor.rowStart.assign(static_cast<size_t>(n + 1), 0);
    for (indexType row = 0; row < n; ++row) {
        for (indexType col = 0; col < n; ++col) {
            if (pattern[static_cast<size_t>(row + col * n)] != 0) {
                factor.columns.push_back(col);
                factor.values.push_back(matrix[static_cast<size_t>(row + col * n)]);
            }
        }
        factor.rowStart[static_cast<size_t>(row + 1)]
            = static_cast<indexType>(factor.columns.size());
    }
    buildIlu0ColumnIndex(factor);
    factorIlu0(factor);
    return factor;
}
//=============================================================================
SparseIlu0Factor
cvodesSparseIlu0Factor(
    NativeProblem* problem, double t, const double* y, const double* fy, double gamma)
{
    SparseJacobianPattern pattern = sparseJacobianPattern(*problem);
    SparseIlu0Factor factor = emptyIlu0Factor(pattern);
    std::vector<double> trial(y, y + problem->n);
    for (indexType col = 0; col < problem->n; ++col) {
        const size_t colIndex = static_cast<size_t>(col);
        double* diagonal = ilu0Entry(factor, col, col);
        if (diagonal != nullptr) {
            *diagonal += 1.0;
        }
        const double saved = trial[colIndex];
        const double step
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(saved));
        trial[colIndex] = saved + step;
        ArrayOfVector out = callProblemFunction(problem->eval, problem->odefun, t, trial.data(),
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
        rhs = applyMass(problem, t, trial.data(), rhs);
        trial[colIndex] = saved;
        for (indexType idx = pattern.columnStart[colIndex];
            idx < pattern.columnStart[static_cast<size_t>(col + 1)]; ++idx) {
            const indexType row = pattern.rows[static_cast<size_t>(idx)];
            double* value = ilu0Entry(factor, row, col);
            if (value != nullptr) {
                *value += -gamma * (rhs[static_cast<size_t>(row)] - fy[row]) / step;
            }
        }
    }
    factorIlu0(factor);
    return factor;
}
//=============================================================================
ArrayOf
cvodesExplicitJacobianValue(NativeProblem* problem, double t, const double* y)
{
    ArrayOf jacobian = getOptionalField(problem->options, "Jacobian");
    if (jacobian.isEmpty()) {
        return ArrayOf::empty();
    }
    if (!jacobian.isFunctionHandle()) {
        return jacobian;
    }
    ArrayOfVector out = callProblemFunction(
        problem->eval, jacobian, t, y, problem->n, currentParameters(problem), 1);
    if (out.empty()) {
        Error(L"Nelson:ode_solvers:jacobianNoValue", L"Jacobian function returned no value.");
    }
    return out[0];
}
//=============================================================================
SparseJacobianPattern
sparsePatternFromMatrix(const ArrayOf& value, indexType n, const std::wstring& typeMessage,
    const std::wstring& sizeMessage)
{
    if (value.rows() != n || value.columns() != n) {
        Error(L"Nelson:ode_solvers:sizeMismatch", sizeMessage);
    }
    if (value.dataClass() != NLS_DOUBLE || !value.isReal() || !value.isSparse()) {
        Error(L"Nelson:ode_solvers:invalidType", typeMessage);
    }
    std::vector<std::vector<indexType>> rowColumns(static_cast<size_t>(n));
    std::vector<std::vector<indexType>> columnRows(static_cast<size_t>(n));
    appendSparsePatternEntries<double>(value, n, rowColumns, columnRows);
    SparseJacobianPattern pattern;
    pattern.n = n;
    pattern.rowStart.assign(static_cast<size_t>(n + 1), 0);
    pattern.columnStart.assign(static_cast<size_t>(n + 1), 0);
    for (indexType row = 0; row < n; ++row) {
        appendSortedUnique(pattern.columns, rowColumns[static_cast<size_t>(row)]);
        pattern.rowStart[static_cast<size_t>(row + 1)]
            = static_cast<indexType>(pattern.columns.size());
    }
    for (indexType col = 0; col < n; ++col) {
        appendSortedUnique(pattern.rows, columnRows[static_cast<size_t>(col)]);
        pattern.columnStart[static_cast<size_t>(col + 1)]
            = static_cast<indexType>(pattern.rows.size());
    }
    return pattern;
}
//=============================================================================
SparseIlu0Factor
cvodesSparseIlu0FactorFromJacobian(const ArrayOf& value, indexType n, double gamma)
{
    using SparseMatrix = Eigen::SparseMatrix<double, 0, signedIndexType>;
    SparseIlu0Factor factor = emptyIlu0Factor(sparsePatternFromMatrix(value, n,
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianRealDoubleSparse",
            L"Jacobian value must be real double sparse."),
        Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jacobianSizeMismatch",
            L"Jacobian matrix has incompatible size.")));
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    for (indexType col = 0; col < n; ++col) {
        double* diagonal = ilu0Entry(factor, col, col);
        if (diagonal != nullptr) {
            *diagonal += 1.0;
        }
    }
    if (matrix != nullptr) {
        for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
            for (SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
                double* entry = ilu0Entry(
                    factor, static_cast<indexType>(it.row()), static_cast<indexType>(it.col()));
                if (entry != nullptr) {
                    *entry += -gamma * static_cast<double>(it.value());
                }
            }
        }
    }
    factorIlu0(factor);
    return factor;
}
//=============================================================================
std::vector<double>
solveIlu0(const SparseIlu0Factor& factor, const std::vector<double>& rhs)
{
    const indexType n = factor.n;
    if (rhs.size() != static_cast<size_t>(n)
        || factor.rowStart.size() != static_cast<size_t>(n + 1)) {
        Error(L"Nelson:ode_solvers:preconditionerSizeMismatch",
            L"Preconditioner has incompatible size.");
    }
    std::vector<double> values = rhs;
    for (indexType row = 0; row < n; ++row) {
        for (indexType idx = factor.rowStart[static_cast<size_t>(row)];
            idx < factor.rowStart[static_cast<size_t>(row + 1)]; ++idx) {
            const indexType col = factor.columns[static_cast<size_t>(idx)];
            if (col < row) {
                values[static_cast<size_t>(row)]
                    -= factor.values[static_cast<size_t>(idx)] * values[static_cast<size_t>(col)];
            }
        }
    }
    for (indexType row = n; row-- > 0;) {
        double diagonal = 0;
        for (indexType idx = factor.rowStart[static_cast<size_t>(row)];
            idx < factor.rowStart[static_cast<size_t>(row + 1)]; ++idx) {
            const indexType col = factor.columns[static_cast<size_t>(idx)];
            const double value = factor.values[static_cast<size_t>(idx)];
            if (col > row) {
                values[static_cast<size_t>(row)] -= value * values[static_cast<size_t>(col)];
            } else if (col == row) {
                diagonal = value;
            }
        }
        values[static_cast<size_t>(row)] /= diagonal;
    }
    return values;
}
//=============================================================================
int
cvodesJacobiSetup(sunrealtype t, N_Vector y, N_Vector fy, sunbooleantype, sunbooleantype* jcurPtr,
    sunrealtype gamma, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        std::vector<double> jac = jacobianDiagonal(problem, t, NV_DATA_S(y), NV_DATA_S(fy));
        problem->preconditionerDiagonal.assign(static_cast<size_t>(problem->n), 1);
        for (indexType k = 0; k < problem->n; ++k) {
            double value = 1.0 - gamma * jac[static_cast<size_t>(k)];
            if (std::abs(value) < std::numeric_limits<double>::epsilon()) {
                value = value < 0 ? -std::numeric_limits<double>::epsilon()
                                  : std::numeric_limits<double>::epsilon();
            }
            problem->preconditionerDiagonal[static_cast<size_t>(k)] = value;
        }
        *jcurPtr = SUNTRUE;
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesBandedSetup(sunrealtype t, N_Vector y, N_Vector fy, sunbooleantype, sunbooleantype* jcurPtr,
    sunrealtype gamma, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        problem->preconditionerMatrix
            = cvodesPreconditionerMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(fy), gamma);
        *jcurPtr = SUNTRUE;
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesBandedSolve(sunrealtype, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype, sunrealtype,
    int, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    if (problem->preconditionerMatrix.size() != static_cast<size_t>(problem->n * problem->n)) {
        return -1;
    }
    try {
        ++problem->preconditionerSolveCount;
        std::vector<double> rhs(NV_DATA_S(r), NV_DATA_S(r) + problem->n);
        std::vector<double> solution = solveDense(problem->preconditionerMatrix, rhs);
        std::copy(solution.begin(), solution.end(), NV_DATA_S(z));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesIlu0Setup(sunrealtype t, N_Vector y, N_Vector fy, sunbooleantype, sunbooleantype* jcurPtr,
    sunrealtype gamma, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        ArrayOf jacobian = cvodesExplicitJacobianValue(problem, t, NV_DATA_S(y));
        if (!jacobian.isEmpty() && jacobian.isSparse()) {
            problem->preconditionerMatrix.clear();
            problem->preconditionerIlu0
                = cvodesSparseIlu0FactorFromJacobian(jacobian, problem->n, gamma);
        } else if (jacobian.isEmpty() && hasSparseJacobianPattern(*problem)
            && densePreconditionerWouldBeLarge(problem->n)) {
            problem->preconditionerMatrix.clear();
            problem->preconditionerIlu0
                = cvodesSparseIlu0Factor(problem, t, NV_DATA_S(y), NV_DATA_S(fy), gamma);
        } else {
            problem->preconditionerMatrix
                = cvodesPreconditionerMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(fy), gamma);
            problem->preconditionerIlu0 = makeIlu0Factor(problem->preconditionerMatrix,
                preconditionerPatternMask(*problem, problem->preconditionerMatrix), problem->n);
        }
        *jcurPtr = SUNTRUE;
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesIlu0Solve(sunrealtype, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype, sunrealtype,
    int, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSolveCount;
        std::vector<double> rhs(NV_DATA_S(r), NV_DATA_S(r) + problem->n);
        std::vector<double> solution = solveIlu0(problem->preconditionerIlu0, rhs);
        std::copy(solution.begin(), solution.end(), NV_DATA_S(z));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
cvodesJacobiSolve(sunrealtype, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype, sunrealtype,
    int, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    if (problem->preconditionerDiagonal.size() != static_cast<size_t>(problem->n)) {
        return -1;
    }
    ++problem->preconditionerSolveCount;
    for (indexType k = 0; k < problem->n; ++k) {
        NV_DATA_S(z)[k] = NV_DATA_S(r)[k] / problem->preconditionerDiagonal[static_cast<size_t>(k)];
    }
    return 0;
}
//=============================================================================
std::vector<double>
idasJacobianMatrix(NativeProblem* problem, double t, const double* y, const double* yp,
    const double* residual, double cj)
{
    if (densePreconditionerWouldBeLarge(problem->n)) {
        Error(L"Nelson:ode_solvers:preconditionerTooLarge",
            L"SUNDIALS preconditioner setup is too large for dense workspace.");
    }
    const JacobianBandwidth bandwidth = patternBandwidth(*problem);
    std::vector<double> matrix(static_cast<size_t>(problem->n * problem->n), 0);
    std::vector<double> trialY(y, y + problem->n);
    std::vector<double> trialYp(yp, yp + problem->n);
    for (indexType col = 0; col < problem->n; ++col) {
        const size_t idx = static_cast<size_t>(col);
        const double savedY = trialY[idx];
        const double stepY
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedY));
        trialY[idx] = savedY + stepY;
        ArrayOfVector outY = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        trialY[idx] = savedY;
        const double savedYp = trialYp[idx];
        const double stepYp
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedYp));
        trialYp[idx] = savedYp + stepYp;
        ArrayOfVector outYp = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        trialYp[idx] = savedYp;
        if (outY.empty() || outYp.empty()) {
            Error(L"Nelson:ode_solvers:implicitFunctionNoValue",
                L"Implicit function returned no value.");
        }
        std::vector<double> ry = realDoubleVector(outY[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        std::vector<double> ryp = realDoubleVector(outYp[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        if (ry.size() != static_cast<size_t>(problem->n)
            || ryp.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:ode_solvers:implicitFunctionSizeMismatch",
                L"Implicit function returned a vector with incompatible size.");
        }
        for (indexType row = 0; row < problem->n; ++row) {
            const bool insideLower = row <= col || row - col <= bandwidth.lower;
            const bool insideUpper = col <= row || col - row <= bandwidth.upper;
            if (insideLower && insideUpper) {
                matrix[static_cast<size_t>(row + col * problem->n)]
                    = (ry[row] - residual[row]) / stepY + cj * (ryp[row] - residual[row]) / stepYp;
            }
        }
    }
    return matrix;
}
//=============================================================================
std::vector<double>
idasJacobianDiagonal(NativeProblem* problem, double t, const double* y, const double* yp,
    const double* residual, double cj)
{
    std::vector<double> diagonal(static_cast<size_t>(problem->n));
    std::vector<double> trialY(y, y + problem->n);
    std::vector<double> trialYp(yp, yp + problem->n);
    for (indexType k = 0; k < problem->n; ++k) {
        const size_t idx = static_cast<size_t>(k);
        const double savedY = trialY[idx];
        const double stepY
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedY));
        trialY[idx] = savedY + stepY;
        ArrayOfVector outY = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        if (outY.empty()) {
            Error(L"Nelson:ode_solvers:implicitFunctionNoValue",
                L"Implicit function returned no value.");
        }
        std::vector<double> ry = realDoubleVector(outY[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        trialY[idx] = savedY;
        const double savedYp = trialYp[idx];
        const double stepYp
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedYp));
        trialYp[idx] = savedYp + stepYp;
        ArrayOfVector outYp = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        if (outYp.empty()) {
            Error(L"Nelson:ode_solvers:implicitFunctionNoValue",
                L"Implicit function returned no value.");
        }
        std::vector<double> ryp = realDoubleVector(outYp[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        trialYp[idx] = savedYp;
        if (ry.size() != static_cast<size_t>(problem->n)
            || ryp.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:ode_solvers:implicitFunctionSizeMismatch",
                L"Implicit function returned a vector with incompatible size.");
        }
        diagonal[idx] = (ry[idx] - residual[k]) / stepY + cj * (ryp[idx] - residual[k]) / stepYp;
    }
    return diagonal;
}
//=============================================================================
SparseIlu0Factor
idasSparseIlu0Factor(NativeProblem* problem, double t, const double* y, const double* yp,
    const double* residual, double cj)
{
    SparseJacobianPattern pattern = sparseJacobianPattern(*problem);
    SparseIlu0Factor factor = emptyIlu0Factor(pattern);
    std::vector<double> trialY(y, y + problem->n);
    std::vector<double> trialYp(yp, yp + problem->n);
    for (indexType col = 0; col < problem->n; ++col) {
        const size_t colIndex = static_cast<size_t>(col);
        const double savedY = trialY[colIndex];
        const double stepY
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedY));
        trialY[colIndex] = savedY + stepY;
        ArrayOfVector outY = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        trialY[colIndex] = savedY;
        const double savedYp = trialYp[colIndex];
        const double stepYp
            = std::sqrt(std::numeric_limits<double>::epsilon()) * std::max(1.0, std::abs(savedYp));
        trialYp[colIndex] = savedYp + stepYp;
        ArrayOfVector outYp = callImplicitFunction(problem->eval, problem->odefun, t, trialY.data(),
            trialYp.data(), problem->n, currentParameters(problem), 1);
        trialYp[colIndex] = savedYp;
        if (outY.empty() || outYp.empty()) {
            Error(L"Nelson:ode_solvers:implicitFunctionNoValue",
                L"Implicit function returned no value.");
        }
        std::vector<double> ry = realDoubleVector(outY[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        std::vector<double> ryp = realDoubleVector(outYp[0],
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:implicitFunctionReturnDoubles",
                L"Implicit function must return doubles."));
        if (ry.size() != static_cast<size_t>(problem->n)
            || ryp.size() != static_cast<size_t>(problem->n)) {
            Error(L"Nelson:ode_solvers:implicitFunctionSizeMismatch",
                L"Implicit function returned a vector with incompatible size.");
        }
        for (indexType idx = pattern.columnStart[colIndex];
            idx < pattern.columnStart[static_cast<size_t>(col + 1)]; ++idx) {
            const indexType row = pattern.rows[static_cast<size_t>(idx)];
            double* value = ilu0Entry(factor, row, col);
            if (value != nullptr) {
                *value = (ry[static_cast<size_t>(row)] - residual[row]) / stepY
                    + cj * (ryp[static_cast<size_t>(row)] - residual[row]) / stepYp;
            }
        }
    }
    factorIlu0(factor);
    return factor;
}
//=============================================================================
int
idasBandedSetup(sunrealtype t, N_Vector y, N_Vector yp, N_Vector rr, sunrealtype cj, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        problem->preconditionerMatrix
            = idasJacobianMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr), cj);
        setSafePivot(problem->preconditionerMatrix, problem->n);
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasBandedSolve(sunrealtype, N_Vector, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype,
    sunrealtype, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    if (problem->preconditionerMatrix.size() != static_cast<size_t>(problem->n * problem->n)) {
        return -1;
    }
    try {
        ++problem->preconditionerSolveCount;
        std::vector<double> rhs(NV_DATA_S(r), NV_DATA_S(r) + problem->n);
        std::vector<double> solution = solveDense(problem->preconditionerMatrix, rhs);
        std::copy(solution.begin(), solution.end(), NV_DATA_S(z));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasIlu0Setup(sunrealtype t, N_Vector y, N_Vector yp, N_Vector rr, sunrealtype cj, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        if (hasSparseJacobianPattern(*problem) && densePreconditionerWouldBeLarge(problem->n)) {
            problem->preconditionerMatrix.clear();
            problem->preconditionerIlu0
                = idasSparseIlu0Factor(problem, t, NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr), cj);
        } else {
            problem->preconditionerMatrix
                = idasJacobianMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr), cj);
            setSafePivot(problem->preconditionerMatrix, problem->n);
            problem->preconditionerIlu0 = makeIlu0Factor(problem->preconditionerMatrix,
                preconditionerPatternMask(*problem, problem->preconditionerMatrix), problem->n);
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasIlu0Solve(sunrealtype, N_Vector, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype,
    sunrealtype, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSolveCount;
        std::vector<double> rhs(NV_DATA_S(r), NV_DATA_S(r) + problem->n);
        std::vector<double> solution = solveIlu0(problem->preconditionerIlu0, rhs);
        std::copy(solution.begin(), solution.end(), NV_DATA_S(z));
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasJacobiSetup(sunrealtype t, N_Vector y, N_Vector yp, N_Vector rr, sunrealtype cj, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        ++problem->preconditionerSetupCount;
        std::vector<double> jac
            = idasJacobianDiagonal(problem, t, NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr), cj);
        problem->preconditionerDiagonal.assign(static_cast<size_t>(problem->n), 1);
        for (indexType k = 0; k < problem->n; ++k) {
            double value = jac[static_cast<size_t>(k)];
            if (std::abs(value) < std::numeric_limits<double>::epsilon()) {
                value = value < 0 ? -std::numeric_limits<double>::epsilon()
                                  : std::numeric_limits<double>::epsilon();
            }
            problem->preconditionerDiagonal[static_cast<size_t>(k)] = value;
        }
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasJacobiSolve(sunrealtype, N_Vector, N_Vector, N_Vector, N_Vector r, N_Vector z, sunrealtype,
    sunrealtype, void* userData)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    if (problem->preconditionerDiagonal.size() != static_cast<size_t>(problem->n)) {
        return -1;
    }
    ++problem->preconditionerSolveCount;
    for (indexType k = 0; k < problem->n; ++k) {
        NV_DATA_S(z)[k] = NV_DATA_S(r)[k] / problem->preconditionerDiagonal[static_cast<size_t>(k)];
    }
    return 0;
}
//=============================================================================
#ifdef NELSON_WITH_SUNDIALS_SPARSE_MATRIX
// Jacobian callbacks for the sparse direct (KLU) linear solver. SUNDIALS has
// no difference-quotient fallback for sparse matrices, so a Jacobian routine
// must be registered whenever a SUNMATRIX_SPARSE is attached. The dense
// (analytic or finite-difference) Jacobian is computed with the existing
// helpers and written in compressed-sparse-column form; the matrix was
// allocated with full n*n capacity so every entry always fits.
void
fillSparseMatrixFromDense(SUNMatrix jacobian, const std::vector<double>& dense, indexType n)
{
    sunindextype* columnStarts = SM_INDEXPTRS_S(jacobian);
    sunindextype* rowIndices = SM_INDEXVALS_S(jacobian);
    double* data = SM_DATA_S(jacobian);
    sunindextype count = 0;
    for (indexType col = 0; col < n; ++col) {
        columnStarts[col] = count;
        for (indexType row = 0; row < n; ++row) {
            rowIndices[count] = static_cast<sunindextype>(row);
            data[count] = dense[static_cast<size_t>(row + col * n)];
            ++count;
        }
    }
    columnStarts[n] = count;
}
//=============================================================================
int
cvodesSparseDirectJacobian(sunrealtype t, N_Vector y, N_Vector fy, SUNMatrix jacobian,
    void* userData, N_Vector, N_Vector, N_Vector)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        std::vector<double> dense = cvodesJacobianMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(fy));
        fillSparseMatrixFromDense(jacobian, dense, problem->n);
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
//=============================================================================
int
idasSparseDirectJacobian(sunrealtype t, sunrealtype cj, N_Vector y, N_Vector yp, N_Vector rr,
    SUNMatrix jacobian, void* userData, N_Vector, N_Vector, N_Vector)
{
    auto* problem = static_cast<NativeProblem*>(userData);
    try {
        std::vector<double> dense
            = idasJacobianMatrix(problem, t, NV_DATA_S(y), NV_DATA_S(yp), NV_DATA_S(rr), cj);
        fillSparseMatrixFromDense(jacobian, dense, problem->n);
        return 0;
    } catch (const NelsonException& e) {
        storeException(problem, e);
        return -1;
    }
}
#endif
//=============================================================================
