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
constexpr size_t SUNDIALS_DENSE_PRECONDITIONER_ELEMENT_LIMIT = 4000000;
//=============================================================================
bool
densePreconditionerWouldBeLarge(indexType n)
{
    if (n <= 0) {
        return false;
    }
    const size_t size = static_cast<size_t>(n);
    return size > SUNDIALS_DENSE_PRECONDITIONER_ELEMENT_LIMIT / size;
}
//=============================================================================
template <class T>
indexType
sparseNonzeroCount(const ArrayOf& value)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    return matrix == nullptr ? 0 : static_cast<indexType>(matrix->nonZeros());
}
//=============================================================================
bool
isRealSparseNumericOrLogical(const ArrayOf& value)
{
    return value.isSparse() && value.isReal()
        && (value.dataClass() == NLS_DOUBLE || value.dataClass() == NLS_LOGICAL);
}
//=============================================================================
indexType
realSparseNonzeroCount(const ArrayOf& value, const std::wstring& message)
{
    if (!isRealSparseNumericOrLogical(value)) {
        Error(L"Nelson:ode_solvers:solverError", message);
    }
    if (value.dataClass() == NLS_LOGICAL) {
        return sparseNonzeroCount<logical>(value);
    }
    return sparseNonzeroCount<double>(value);
}
//=============================================================================
template <class T>
std::vector<double>
sparseDiagonalValues(const ArrayOf& value, indexType n)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    std::vector<double> diagonal(static_cast<size_t>(n), 0);
    if (matrix == nullptr) {
        return diagonal;
    }
    for (indexType k = 0; k < n; ++k) {
        diagonal[static_cast<size_t>(k)] = static_cast<double>(
            matrix->coeff(static_cast<signedIndexType>(k), static_cast<signedIndexType>(k)));
    }
    return diagonal;
}
//=============================================================================
template <class T>
std::vector<double>
sparseMatrixValues(const ArrayOf& value, indexType n)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    std::vector<double> values(static_cast<size_t>(n * n), 0);
    if (matrix == nullptr) {
        return values;
    }
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            values[static_cast<size_t>(it.row() + it.col() * n)] = static_cast<double>(it.value());
        }
    }
    return values;
}
//=============================================================================
std::vector<double>
matrixValues(const ArrayOf& value, indexType n, const std::wstring& typeMessage,
    const std::wstring& sizeMessage)
{
    if (value.rows() != n || value.columns() != n) {
        Error(L"Nelson:ode_solvers:sizeMismatch", sizeMessage);
    }
    if (value.isSparse()) {
        if (value.dataClass() != NLS_DOUBLE || !value.isReal()) {
            Error(L"Nelson:ode_solvers:invalidType", typeMessage);
        }
        return sparseMatrixValues<double>(value, n);
    }
    std::vector<double> values = realDoubleVector(value, typeMessage);
    if (values.size() != static_cast<size_t>(n * n)) {
        Error(L"Nelson:ode_solvers:sizeMismatch", sizeMessage);
    }
    return values;
}
//=============================================================================
std::vector<double>
matrixDiagonalValues(const ArrayOf& value, indexType n, const std::wstring& typeMessage,
    const std::wstring& sizeMessage)
{
    if (value.rows() != n || value.columns() != n) {
        Error(L"Nelson:ode_solvers:sizeMismatch", sizeMessage);
    }
    if (value.isSparse()) {
        if (value.dataClass() != NLS_DOUBLE || !value.isReal()) {
            Error(L"Nelson:ode_solvers:invalidType", typeMessage);
        }
        return sparseDiagonalValues<double>(value, n);
    }
    std::vector<double> values = realDoubleVector(value, typeMessage);
    if (values.size() != static_cast<size_t>(n * n)) {
        Error(L"Nelson:ode_solvers:sizeMismatch", sizeMessage);
    }
    std::vector<double> diagonal(static_cast<size_t>(n));
    for (indexType k = 0; k < n; ++k) {
        diagonal[static_cast<size_t>(k)] = values[static_cast<size_t>(k + k * n)];
    }
    return diagonal;
}
//=============================================================================
struct JacobianBandwidth
{
    indexType lower { 0 };
    indexType upper { 0 };
};
//=============================================================================
struct SparseJacobianPattern
{
    indexType n { 0 };
    std::vector<indexType> rowStart;
    std::vector<indexType> columns;
    std::vector<indexType> columnStart;
    std::vector<indexType> rows;
};
//=============================================================================
void
appendPatternEntry(std::vector<std::vector<indexType>>& rowColumns,
    std::vector<std::vector<indexType>>& columnRows, indexType row, indexType col)
{
    rowColumns[static_cast<size_t>(row)].push_back(col);
    columnRows[static_cast<size_t>(col)].push_back(row);
}
//=============================================================================
template <class T>
void
appendSparsePatternEntries(const ArrayOf& value, indexType n,
    std::vector<std::vector<indexType>>& rowColumns,
    std::vector<std::vector<indexType>>& columnRows)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    if (matrix == nullptr) {
        return;
    }
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            appendPatternEntry(rowColumns, columnRows, static_cast<indexType>(it.row()),
                static_cast<indexType>(it.col()));
        }
    }
    for (indexType k = 0; k < n; ++k) {
        appendPatternEntry(rowColumns, columnRows, k, k);
    }
}
//=============================================================================
void
appendDensePatternEntries(const ArrayOf& value, indexType n,
    std::vector<std::vector<indexType>>& rowColumns,
    std::vector<std::vector<indexType>>& columnRows)
{
    const std::vector<double> values = realNumericVector(value,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:jpatternNumericLogical", L"JPattern must be numeric or logical."),
        true);
    for (indexType col = 0; col < n; ++col) {
        for (indexType row = 0; row < n; ++row) {
            if (values[static_cast<size_t>(row + col * n)] != 0 || row == col) {
                appendPatternEntry(rowColumns, columnRows, row, col);
            }
        }
    }
}
//=============================================================================
void
appendSortedUnique(std::vector<indexType>& destination, std::vector<indexType>& values)
{
    std::sort(values.begin(), values.end());
    values.erase(std::unique(values.begin(), values.end()), values.end());
    destination.insert(destination.end(), values.begin(), values.end());
}
//=============================================================================
SparseJacobianPattern
sparseJacobianPattern(const NativeProblem& problem)
{
    ArrayOf patternValue = getOptionalField(problem.options, "JPattern");
    if (patternValue.isEmpty()) {
        Error(L"Nelson:ode_solvers:sparsePreconditionerRequiresJPattern",
            L"Sparse preconditioner requires JPattern.");
    }
    if (patternValue.rows() != problem.n || patternValue.columns() != problem.n) {
        Error(
            L"Nelson:ode_solvers:jPatternSizeMismatch", L"JPattern matrix has incompatible size.");
    }
    std::vector<std::vector<indexType>> rowColumns(static_cast<size_t>(problem.n));
    std::vector<std::vector<indexType>> columnRows(static_cast<size_t>(problem.n));
    if (patternValue.isSparse()) {
        if (!isRealSparseNumericOrLogical(patternValue)) {
            Error(L"Nelson:ode_solvers:jpatternNumericLogical",
                L"JPattern must be numeric or logical.");
        }
        if (patternValue.dataClass() == NLS_LOGICAL) {
            appendSparsePatternEntries<logical>(patternValue, problem.n, rowColumns, columnRows);
        } else {
            appendSparsePatternEntries<double>(patternValue, problem.n, rowColumns, columnRows);
        }
    } else {
        appendDensePatternEntries(patternValue, problem.n, rowColumns, columnRows);
    }
    SparseJacobianPattern pattern;
    pattern.n = problem.n;
    pattern.rowStart.assign(static_cast<size_t>(problem.n + 1), 0);
    pattern.columnStart.assign(static_cast<size_t>(problem.n + 1), 0);
    for (indexType row = 0; row < problem.n; ++row) {
        appendSortedUnique(pattern.columns, rowColumns[static_cast<size_t>(row)]);
        pattern.rowStart[static_cast<size_t>(row + 1)]
            = static_cast<indexType>(pattern.columns.size());
    }
    for (indexType col = 0; col < problem.n; ++col) {
        appendSortedUnique(pattern.rows, columnRows[static_cast<size_t>(col)]);
        pattern.columnStart[static_cast<size_t>(col + 1)]
            = static_cast<indexType>(pattern.rows.size());
    }
    return pattern;
}
//=============================================================================
template <class T>
JacobianBandwidth
sparsePatternBandwidth(const ArrayOf& value)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    JacobianBandwidth bandwidth;
    if (matrix == nullptr) {
        return bandwidth;
    }
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            if (it.row() >= it.col()) {
                bandwidth.lower = std::max<indexType>(
                    bandwidth.lower, static_cast<indexType>(it.row() - it.col()));
            } else {
                bandwidth.upper = std::max<indexType>(
                    bandwidth.upper, static_cast<indexType>(it.col() - it.row()));
            }
        }
    }
    return bandwidth;
}
//=============================================================================
JacobianBandwidth
patternBandwidth(const NativeProblem& problem)
{
    ArrayOf pattern = getOptionalField(problem.options, "JPattern");
    if (pattern.isEmpty()) {
        return { problem.n - 1, problem.n - 1 };
    }
    if (pattern.rows() != problem.n || pattern.columns() != problem.n) {
        Error(
            L"Nelson:ode_solvers:jPatternSizeMismatch", L"JPattern matrix has incompatible size.");
    }
    if (pattern.isSparse()) {
        if (!isRealSparseNumericOrLogical(pattern)) {
            Error(L"Nelson:ode_solvers:jpatternNumericLogical",
                L"JPattern must be numeric or logical.");
        }
        if (pattern.dataClass() == NLS_LOGICAL) {
            return sparsePatternBandwidth<logical>(pattern);
        }
        return sparsePatternBandwidth<double>(pattern);
    }
    const std::vector<double> values = realNumericVector(pattern,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:jpatternNumericLogical", L"JPattern must be numeric or logical."),
        true);
    JacobianBandwidth bandwidth;
    for (indexType col = 0; col < problem.n; ++col) {
        for (indexType row = 0; row < problem.n; ++row) {
            if (values[static_cast<size_t>(row + col * problem.n)] != 0) {
                if (row >= col) {
                    bandwidth.lower = std::max<indexType>(bandwidth.lower, row - col);
                } else {
                    bandwidth.upper = std::max<indexType>(bandwidth.upper, col - row);
                }
            }
        }
    }
    return bandwidth;
}
//=============================================================================
void
zeroOutsideBand(std::vector<double>& values, indexType n, const JacobianBandwidth& bandwidth)
{
    for (indexType col = 0; col < n; ++col) {
        for (indexType row = 0; row < n; ++row) {
            const bool outsideLower = row >= col && row - col > bandwidth.lower;
            const bool outsideUpper = col > row && col - row > bandwidth.upper;
            if (outsideLower || outsideUpper) {
                values[static_cast<size_t>(row + col * n)] = 0;
            }
        }
    }
}
//=============================================================================
void
setPatternDiagonal(std::vector<unsigned char>& pattern, indexType n)
{
    for (indexType k = 0; k < n; ++k) {
        pattern[static_cast<size_t>(k + k * n)] = 1;
    }
}
//=============================================================================
template <class T>
std::vector<unsigned char>
sparsePatternMask(const ArrayOf& value, indexType n)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* matrix = static_cast<const SparseMatrix*>(value.sparseDataPointer());
    std::vector<unsigned char> pattern(static_cast<size_t>(n * n), 0);
    if (matrix == nullptr) {
        setPatternDiagonal(pattern, n);
        return pattern;
    }
    for (signedIndexType col = 0; col < matrix->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*matrix, col); it; ++it) {
            pattern[static_cast<size_t>(it.row() + it.col() * n)] = 1;
        }
    }
    setPatternDiagonal(pattern, n);
    return pattern;
}
//=============================================================================
std::vector<unsigned char>
jacobianPatternMask(const NativeProblem& problem)
{
    ArrayOf patternValue = getOptionalField(problem.options, "JPattern");
    std::vector<unsigned char> pattern(static_cast<size_t>(problem.n * problem.n), 0);
    if (patternValue.isEmpty()) {
        std::fill(pattern.begin(), pattern.end(), 1);
        return pattern;
    }
    if (patternValue.rows() != problem.n || patternValue.columns() != problem.n) {
        Error(
            L"Nelson:ode_solvers:jPatternSizeMismatch", L"JPattern matrix has incompatible size.");
    }
    if (patternValue.isSparse()) {
        if (!isRealSparseNumericOrLogical(patternValue)) {
            Error(L"Nelson:ode_solvers:jpatternNumericLogical",
                L"JPattern must be numeric or logical.");
        }
        if (patternValue.dataClass() == NLS_LOGICAL) {
            return sparsePatternMask<logical>(patternValue, problem.n);
        }
        return sparsePatternMask<double>(patternValue, problem.n);
    }
    const std::vector<double> values = realNumericVector(patternValue,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:jpatternNumericLogical", L"JPattern must be numeric or logical."),
        true);
    for (indexType col = 0; col < problem.n; ++col) {
        for (indexType row = 0; row < problem.n; ++row) {
            if (values[static_cast<size_t>(row + col * problem.n)] != 0) {
                pattern[static_cast<size_t>(row + col * problem.n)] = 1;
            }
        }
    }
    setPatternDiagonal(pattern, problem.n);
    return pattern;
}
//=============================================================================
std::vector<unsigned char>
preconditionerPatternMask(const NativeProblem& problem, const std::vector<double>& matrix)
{
    ArrayOf patternValue = getOptionalField(problem.options, "JPattern");
    if (!patternValue.isEmpty()) {
        return jacobianPatternMask(problem);
    }
    std::vector<unsigned char> pattern(static_cast<size_t>(problem.n * problem.n), 0);
    for (indexType col = 0; col < problem.n; ++col) {
        for (indexType row = 0; row < problem.n; ++row) {
            const size_t idx = static_cast<size_t>(row + col * problem.n);
            pattern[idx] = matrix[idx] != 0;
        }
    }
    setPatternDiagonal(pattern, problem.n);
    return pattern;
}
//=============================================================================
bool
hasSparseJacobianPattern(const NativeProblem& problem)
{
    ArrayOf pattern = getOptionalField(problem.options, "JPattern");
    if (pattern.isEmpty() || pattern.elementCount() <= 1) {
        return false;
    }
    if (pattern.rows() != problem.n || pattern.columns() != problem.n) {
        Error(
            L"Nelson:ode_solvers:jPatternSizeMismatch", L"JPattern matrix has incompatible size.");
    }
    if (pattern.isSparse()) {
        const indexType nonzero = realSparseNonzeroCount(pattern,
            Nelson::resolveMessageTemplate(L"Nelson:ode_solvers:jpatternNumericLogical",
                L"JPattern must be numeric or logical."));
        return nonzero > 0 && nonzero < pattern.elementCount();
    }
    const std::vector<double> values = realNumericVector(pattern,
        Nelson::resolveMessageTemplate(
            L"Nelson:ode_solvers:jpatternNumericLogical", L"JPattern must be numeric or logical."),
        true);
    size_t nonzero = 0;
    for (double value : values) {
        nonzero += value != 0;
    }
    return nonzero > 0 && nonzero < values.size();
}
//=============================================================================
