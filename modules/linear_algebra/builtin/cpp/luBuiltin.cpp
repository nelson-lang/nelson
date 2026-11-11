//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "luBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "LUMatrixFactorization.hpp"
#include "SparseArrayOf.hpp"
#include <Eigen/Sparse>
#include <Eigen/OrderingMethods>
#include <algorithm>
#include <cmath>
#include <cwctype>
#include <limits>
#include <numeric>
#include <type_traits>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
using Index = signedIndexType;
//=============================================================================
struct LUOptions
{
    bool asVector = false;
    bool hasThreshold = false;
    std::vector<double> thresholds;
};
//=============================================================================
// Pivot rules: partial pivoting in place, or sparsity first with a column order.
struct SparsePivotRule
{
    bool reorder = false;
    bool symmetric = false;
    bool scaleRows = false;
    double pivotTolerance = 1.0;
    double diagonalTolerance = 0.001;
};
//=============================================================================
template <class T> struct SparseLUResult
{
    using Real = typename Eigen::NumTraits<T>::Real;
    Index rows = 0;
    Index cols = 0;
    Index order = 0;
    std::vector<Index> rowOrder; // position -> original row
    std::vector<Index> rowPosition; // original row -> position
    std::vector<Index> colOrder; // position -> original column
    std::vector<Real> rowScale;
    std::vector<Index> lStart, lRows;
    std::vector<T> lValues; // first entry of each column is the pivot row
    std::vector<Eigen::Triplet<T, Index>> uTriplets;
};
//=============================================================================
template <class T>
inline typename Eigen::NumTraits<T>::Real
entryMagnitude(const T& value)
{
    return std::abs(value);
}
//=============================================================================
// Scaling uses |re| + |im| for complex entries.
template <class T>
inline typename Eigen::NumTraits<T>::Real
scaleMagnitude(const T& value)
{
    if constexpr (Eigen::NumTraits<T>::IsComplex) {
        return std::abs(value.real()) + std::abs(value.imag());
    } else {
        return std::abs(value);
    }
}
//=============================================================================
// A real-valued complex pivot divides each part on its own, so NaN stays in one part.
template <class T>
inline T
divideByPivot(const T& value, const T& pivot)
{
    if constexpr (Eigen::NumTraits<T>::IsComplex) {
        if (pivot.imag() == 0) {
            return T(value.real() / pivot.real(), value.imag() / pivot.real());
        }
    }
    return value / pivot;
}
//=============================================================================
template <class T>
static bool
isMostlySymmetric(const Eigen::SparseMatrix<T, 0, Index>& A)
{
    if (A.rows() != A.cols() || A.rows() == 0) {
        return false;
    }
    Index n = A.cols();
    Index diagonal = 0;
    Index offDiagonal = 0;
    Index matched = 0;
    Eigen::SparseMatrix<T, 0, Index> At = A.transpose();
    for (Index j = 0; j < n; ++j) {
        Index a = A.outerIndexPtr()[j];
        Index aEnd = A.outerIndexPtr()[j + 1];
        Index b = At.outerIndexPtr()[j];
        Index bEnd = At.outerIndexPtr()[j + 1];
        for (Index p = a; p < aEnd; ++p) {
            Index i = A.innerIndexPtr()[p];
            if (i == j) {
                diagonal++;
                continue;
            }
            offDiagonal++;
            while (b < bEnd && At.innerIndexPtr()[b] < i) {
                b++;
            }
            if (b < bEnd && At.innerIndexPtr()[b] == i) {
                matched++;
            }
        }
    }
    bool symmetricPattern = offDiagonal == 0 || 2 * matched >= offDiagonal;
    return symmetricPattern && 10 * diagonal >= 9 * n;
}
//=============================================================================
template <class T>
static std::vector<Index>
fillReducingColumnOrder(Eigen::SparseMatrix<T, 0, Index>& A, bool symmetric)
{
    std::vector<Index> order(A.cols());
    if (A.cols() == 0 || A.rows() == 0) {
        std::iota(order.begin(), order.end(), 0);
        return order;
    }
    Eigen::PermutationMatrix<Eigen::Dynamic, Eigen::Dynamic, Index> permutation;
    if (symmetric) {
        Eigen::AMDOrdering<Index> ordering;
        ordering(A, permutation);
        for (Index k = 0; k < A.cols(); ++k) {
            order[k] = permutation.indices()(k);
        }
    } else {
        Eigen::COLAMDOrdering<Index> ordering;
        ordering(A, permutation);
        for (Index k = 0; k < A.cols(); ++k) {
            order[permutation.indices()(k)] = k;
        }
    }
    return order;
}
//=============================================================================
// Left-looking sparse LU: each column is solved against the previous L columns.
template <class T>
static SparseLUResult<T>
sparseLU(const Eigen::SparseMatrix<T, 0, Index>& input, SparsePivotRule rule)
{
    using Real = typename Eigen::NumTraits<T>::Real;
    SparseLUResult<T> result;
    Eigen::SparseMatrix<T, 0, Index> A(input);
    A.makeCompressed();
    const Index m = A.rows();
    const Index n = A.cols();
    result.rows = m;
    result.cols = n;
    result.order = std::min(m, n);

    if (rule.scaleRows) {
        result.rowScale.assign(m, Real(0));
        for (Index p = 0; p < A.nonZeros(); ++p) {
            result.rowScale[A.innerIndexPtr()[p]] += scaleMagnitude(A.valuePtr()[p]);
        }
        for (Index i = 0; i < m; ++i) {
            if (result.rowScale[i] == Real(0) || std::isnan(result.rowScale[i])) {
                result.rowScale[i] = Real(1);
            }
        }
        for (Index p = 0; p < A.nonZeros(); ++p) {
            A.valuePtr()[p] /= result.rowScale[A.innerIndexPtr()[p]];
        }
    }

    std::vector<Index> rowCount;
    std::vector<Index> frontRows;
    std::vector<Index> frontSlot;
    if (rule.reorder) {
        rule.symmetric = isMostlySymmetric<T>(A);
        result.colOrder = fillReducingColumnOrder(A, rule.symmetric);
        rowCount.assign(m, 0);
        // rows still to pivot, the last one fills the slot of each pivot row
        frontRows.resize(m);
        frontSlot.resize(m);
        std::iota(frontRows.begin(), frontRows.end(), 0);
        std::iota(frontSlot.begin(), frontSlot.end(), 0);
        for (Index p = 0; p < A.nonZeros(); ++p) {
            rowCount[A.innerIndexPtr()[p]]++;
        }
    } else {
        result.colOrder.resize(n);
        std::iota(result.colOrder.begin(), result.colOrder.end(), 0);
    }

    std::vector<Index>& rowOrder = result.rowOrder;
    std::vector<Index>& rowPosition = result.rowPosition;
    rowOrder.resize(m);
    rowPosition.resize(m);
    std::iota(rowOrder.begin(), rowOrder.end(), 0);
    std::iota(rowPosition.begin(), rowPosition.end(), 0);
    std::vector<Index> pivotStep(m, -1);
    // steps whose L column holds NaN or Inf, where a zero still propagates
    std::vector<char> nonFiniteColumn;
    std::vector<Index> visited(m, -1);
    std::vector<T> x(m, T(0));
    std::vector<Index> postOrder;
    std::vector<Index> stackNode;
    std::vector<Index> stackNext;
    postOrder.reserve(m);
    std::vector<Index>& lStart = result.lStart;
    std::vector<Index>& lRows = result.lRows;
    std::vector<T>& lValues = result.lValues;
    lStart.reserve(result.order + 1);
    lStart.push_back(0);
    lRows.reserve(A.nonZeros() + result.order);
    lValues.reserve(A.nonZeros() + result.order);
    result.uTriplets.reserve(A.nonZeros() + result.order);

    const Index* outer = A.outerIndexPtr();
    const Index* inner = A.innerIndexPtr();
    const T* values = A.valuePtr();

    for (Index k = 0; k < n; ++k) {
        const Index column = result.colOrder[k];
        // rows reached from this column through the L graph, in post order
        postOrder.clear();
        for (Index p = outer[column]; p < outer[column + 1]; ++p) {
            Index start = inner[p];
            if (visited[start] == k) {
                continue;
            }
            visited[start] = k;
            stackNode.assign(1, start);
            stackNext.assign(1, 0);
            while (!stackNode.empty()) {
                Index node = stackNode.back();
                Index step = pivotStep[node];
                Index& next = stackNext.back();
                bool pushed = false;
                if (step >= 0) {
                    Index from = lStart[step] + 1 + next;
                    Index to = lStart[step + 1];
                    for (; from < to; ++from) {
                        next++;
                        Index child = lRows[from];
                        if (visited[child] != k) {
                            visited[child] = k;
                            stackNode.push_back(child);
                            stackNext.push_back(0);
                            pushed = true;
                            break;
                        }
                    }
                }
                if (!pushed) {
                    postOrder.push_back(node);
                    stackNode.pop_back();
                    stackNext.pop_back();
                }
            }
        }
        for (Index p = outer[column]; p < outer[column + 1]; ++p) {
            x[inner[p]] = values[p];
            if (rule.reorder) {
                rowCount[inner[p]]--;
            }
        }
        // sparse triangular solve in topological order
        for (auto it = postOrder.rbegin(); it != postOrder.rend(); ++it) {
            Index step = pivotStep[*it];
            if (step < 0) {
                continue;
            }
            T value = x[*it];
            if (value == T(0) && !nonFiniteColumn[step]) {
                continue;
            }
            for (Index q = lStart[step] + 1; q < lStart[step + 1]; ++q) {
                x[lRows[q]] -= lValues[q] * value;
            }
        }
        Real maxMagnitude = Real(0);
        bool hasNaN = false;
        for (Index node : postOrder) {
            if (pivotStep[node] >= 0) {
                if (x[node] != T(0)) {
                    result.uTriplets.emplace_back(pivotStep[node], k, x[node]);
                }
                x[node] = T(0);
            } else {
                Real magnitude = entryMagnitude(x[node]);
                hasNaN = hasNaN || std::isnan(magnitude);
                maxMagnitude = std::max(maxMagnitude, magnitude);
            }
        }
        if (k >= m) {
            continue;
        }
        Index pivotRow = rowOrder[k];
        if (!rule.reorder) {
            if (hasNaN) {
                // the NaN candidate with the lowest row index wins the partial pivot search
                Index best = -1;
                for (Index node : postOrder) {
                    if (pivotStep[node] < 0 && std::isnan(entryMagnitude(x[node]))
                        && (best < 0 || node < best)) {
                        best = node;
                    }
                }
                pivotRow = best;
            } else if (maxMagnitude > Real(0)) {
                Real current = visited[pivotRow] == k ? entryMagnitude(x[pivotRow]) : Real(0);
                if (!(current > Real(0) && current >= rule.pivotTolerance * maxMagnitude)) {
                    Index best = -1;
                    for (Index node : postOrder) {
                        if (pivotStep[node] < 0 && entryMagnitude(x[node]) == maxMagnitude
                            && (best < 0 || rowPosition[node] < rowPosition[best])) {
                            best = node;
                        }
                    }
                    pivotRow = best;
                }
            }
        } else if (maxMagnitude > Real(0) || hasNaN) {
            // a NaN in the column makes every threshold test fail
            Real reference = hasNaN ? std::numeric_limits<Real>::quiet_NaN() : maxMagnitude;
            Index diagonalRow = column < m ? column : -1;
            bool diagonalCandidate
                = diagonalRow >= 0 && visited[diagonalRow] == k && pivotStep[diagonalRow] < 0;
            Real diagonal = diagonalCandidate ? entryMagnitude(x[diagonalRow]) : Real(0);
            if (rule.symmetric && diagonal > Real(0)
                && diagonal >= rule.diagonalTolerance * reference) {
                pivotRow = diagonalRow;
            } else {
                Index best = -1;
                Real bestMagnitude = Real(0);
                Real threshold = static_cast<Real>(rule.pivotTolerance) * reference;
                for (Index node : postOrder) {
                    if (pivotStep[node] >= 0) {
                        continue;
                    }
                    Real magnitude = entryMagnitude(x[node]);
                    if (magnitude == Real(0) || !(magnitude >= threshold)) {
                        continue;
                    }
                    bool better = best < 0 || rowCount[node] < rowCount[best]
                        || (rowCount[node] == rowCount[best]
                            && (magnitude > bestMagnitude
                                || (magnitude == bestMagnitude
                                    && rowPosition[node] < rowPosition[best])));
                    if (better) {
                        best = node;
                        bestMagnitude = magnitude;
                    }
                }
                if (best < 0) {
                    // no row passed: the stored diagonal when symmetric, else the sparsest row
                    if (rule.symmetric && diagonalCandidate) {
                        best = diagonalRow;
                    } else {
                        for (Index node : postOrder) {
                            if (pivotStep[node] < 0 && x[node] != T(0)
                                && (best < 0 || rowCount[node] < rowCount[best]
                                    || (rowCount[node] == rowCount[best]
                                        && frontSlot[node] < frontSlot[best]))) {
                                best = node;
                            }
                        }
                    }
                }
                pivotRow = best;
            }
        }
        T pivot = visited[pivotRow] == k ? x[pivotRow] : T(0);
        lRows.push_back(pivotRow);
        lValues.push_back(T(1));
        for (Index node : postOrder) {
            if (pivotStep[node] < 0 && node != pivotRow && x[node] != T(0)) {
                lRows.push_back(node);
                lValues.push_back(divideByPivot(x[node], pivot));
            }
        }
        lStart.push_back(static_cast<Index>(lRows.size()));
        bool nonFinite = false;
        for (Index q = lStart[k] + 1; q < lStart[k + 1] && !nonFinite; ++q) {
            nonFinite = !std::isfinite(entryMagnitude(lValues[q]));
        }
        nonFiniteColumn.push_back(nonFinite ? 1 : 0);
        if (rule.reorder) {
            // rows below the pivot may gain the pivot row's remaining entries
            Index remaining = n - k - 1;
            for (Index q = lStart[k] + 1; q < lStart[k + 1]; ++q) {
                Index& count = rowCount[lRows[q]];
                count = std::min(remaining, count + rowCount[pivotRow]);
            }
        }
        if (pivot != T(0)) {
            result.uTriplets.emplace_back(k, k, pivot);
        }
        for (Index node : postOrder) {
            x[node] = T(0);
        }
        pivotStep[pivotRow] = k;
        if (rule.reorder) {
            Index slot = frontSlot[pivotRow];
            Index last = frontRows[m - 1 - k];
            frontRows[slot] = last;
            frontSlot[last] = slot;
        }
        Index from = rowPosition[pivotRow];
        Index displaced = rowOrder[k];
        rowOrder[k] = pivotRow;
        rowOrder[from] = displaced;
        rowPosition[pivotRow] = k;
        rowPosition[displaced] = from;
    }
    return result;
}
//=============================================================================
template <class T>
static Eigen::SparseMatrix<T, 0, Index>*
fromTriplets(Index rows, Index cols, const std::vector<Eigen::Triplet<T, Index>>& triplets)
{
    auto* matrix = new Eigen::SparseMatrix<T, 0, Index>(rows, cols);
    matrix->setFromTriplets(triplets.begin(), triplets.end());
    matrix->makeCompressed();
    return matrix;
}
//=============================================================================
static ArrayOf
permutationVector(const std::vector<Index>& order)
{
    Index count = static_cast<Index>(order.size());
    double* values
        = (double*)ArrayOf::allocateArrayOf(NLS_DOUBLE, count, stringVector(), false);
    for (Index k = 0; k < count; ++k) {
        values[k] = static_cast<double>(order[k] + 1);
    }
    return ArrayOf(NLS_DOUBLE, Dimensions(1, count), values);
}
//=============================================================================
// Real sparse matrix with ones at (k, order[k]), or (order[k], k) when transposed.
template <class Real>
static ArrayOf
permutationMatrix(NelsonType realClass, const std::vector<Index>& order, bool transposed)
{
    Index count = static_cast<Index>(order.size());
    std::vector<Eigen::Triplet<Real, Index>> triplets;
    triplets.reserve(count);
    for (Index k = 0; k < count; ++k) {
        if (transposed) {
            triplets.emplace_back(order[k], k, Real(1));
        } else {
            triplets.emplace_back(k, order[k], Real(1));
        }
    }
    return makeSparseResult<Real>(
        realClass, Dimensions(count, count), fromTriplets<Real>(count, count, triplets));
}
//=============================================================================
template <class T>
static ArrayOfVector
sparseLUFactorization(const ArrayOf& A, int nLhs, const LUOptions& options)
{
    using Real = typename Eigen::NumTraits<T>::Real;
    using SparseMatrix = Eigen::SparseMatrix<T, 0, Index>;
    const NelsonType realClass = std::is_same<Real, double>::value ? NLS_DOUBLE : NLS_SINGLE;
    const auto* spmat = static_cast<const SparseMatrix*>(A.sparseDataPointer());

    SparsePivotRule rule;
    if (nLhs >= 4) {
        rule.reorder = true;
        rule.scaleRows = nLhs == 5;
        rule.pivotTolerance = 0.1;
        if (options.hasThreshold) {
            rule.pivotTolerance = options.thresholds[0];
            if (options.thresholds.size() > 1) {
                rule.diagonalTolerance = options.thresholds[1];
            }
        }
    } else if (options.hasThreshold) {
        rule.pivotTolerance = options.thresholds[0];
    }
    SparseLUResult<T> lu = sparseLU<T>(*spmat, rule);
    const Index rows = lu.rows;
    const Index cols = lu.cols;
    const Index order = lu.order;

    std::vector<Eigen::Triplet<T, Index>> lTriplets;
    lTriplets.reserve(lu.lRows.size());
    for (Index c = 0; c < order; ++c) {
        for (Index q = lu.lStart[c]; q < lu.lStart[c + 1]; ++q) {
            Index row = nLhs == 2 ? lu.lRows[q] : lu.rowPosition[lu.lRows[q]];
            if (nLhs <= 1 && q == lu.lStart[c]) {
                continue;
            }
            lTriplets.emplace_back(row, c, lu.lValues[q]);
        }
    }

    ArrayOfVector retval;
    if (nLhs <= 1) {
        lTriplets.insert(lTriplets.end(), lu.uTriplets.begin(), lu.uTriplets.end());
        retval << makeSparseResult<T>(
            A.dataClass(), Dimensions(rows, cols), fromTriplets<T>(rows, cols, lTriplets));
        return retval;
    }
    retval << makeSparseResult<T>(
        A.dataClass(), Dimensions(rows, order), fromTriplets<T>(rows, order, lTriplets));
    retval << makeSparseResult<T>(
        A.dataClass(), Dimensions(order, cols), fromTriplets<T>(order, cols, lu.uTriplets));
    if (nLhs >= 3) {
        retval << (options.asVector ? permutationVector(lu.rowOrder)
                                    : permutationMatrix<Real>(realClass, lu.rowOrder, false));
    }
    if (nLhs >= 4) {
        retval << (options.asVector ? permutationVector(lu.colOrder)
                                    : permutationMatrix<Real>(realClass, lu.colOrder, true));
    }
    if (nLhs >= 5) {
        std::vector<Eigen::Triplet<Real, Index>> dTriplets;
        dTriplets.reserve(rows);
        for (Index i = 0; i < rows; ++i) {
            dTriplets.emplace_back(i, i, lu.rowScale[i]);
        }
        retval << makeSparseResult<Real>(
            realClass, Dimensions(rows, rows), fromTriplets<Real>(rows, rows, dTriplets));
    }
    return retval;
}
//=============================================================================
// true for 'vector', false for 'matrix' (any case, any leading part)
static bool
parseOutputForm(const ArrayOf& arg)
{
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::wstring text = arg.asWideString();
        std::transform(text.begin(), text.end(), text.begin(), ::towlower);
        if (!text.empty()) {
            if (std::wstring(L"vector").compare(0, text.size(), text) == 0) {
                return true;
            }
            if (std::wstring(L"matrix").compare(0, text.size(), text) == 0) {
                return false;
            }
        }
    }
    Error(L"Nelson:lu:invalidOption", L"The output form must be 'matrix' or 'vector'.");
    return false;
}
//=============================================================================
static bool
isThresholdCandidate(const ArrayOf& arg)
{
    switch (arg.dataClass()) {
    case NLS_DOUBLE:
    case NLS_DCOMPLEX:
    case NLS_SINGLE:
    case NLS_SCOMPLEX:
        return true;
    default:
        return arg.isIntegerType();
    }
}
//=============================================================================
static std::vector<double>
parseThreshold(const ArrayOf& arg, int nLhs)
{
    if (arg.isIntegerType()) {
        Error(L"Nelson:lu:pivotThresholdType", L"The pivot threshold must be double or single.");
    }
    indexType count = arg.elementCount();
    if (nLhs <= 3 && count != 1) {
        Error(L"Nelson:lu:nonScalarPivotThreshold",
            L"The pivot threshold must be a scalar with three outputs or fewer.");
    }
    if (count != 1 && count != 2) {
        Error(L"Nelson:lu:pivotThresholdSize",
            L"The pivot threshold must be a scalar or a vector of two elements.");
    }
    ArrayOf value(arg);
    if (value.isSparse()) {
        value.makeDense();
    }
    std::vector<double> thresholds(count);
    if (value.isComplex()) {
        value.promoteType(NLS_DCOMPLEX);
        const double* data = (const double*)value.dataPointer();
        for (indexType k = 0; k < count; ++k) {
            thresholds[k] = data[2 * k + 1] == 0 ? data[2 * k] : std::nan("");
        }
    } else {
        value.promoteType(NLS_DOUBLE);
        const double* data = (const double*)value.dataPointer();
        std::copy(data, data + count, thresholds.begin());
    }
    for (double t : thresholds) {
        if (!(t >= 0 && t <= 1)) {
            Error(L"Nelson:lu:pivotThreshold", L"The pivot threshold must be between 0 and 1.");
        }
    }
    return thresholds;
}
//=============================================================================
static LUOptions
parseOptions(const ArrayOfVector& argIn, int nLhs, bool isSparse)
{
    LUOptions options;
    size_t nRhs = argIn.size();
    if (nRhs == 3 && !isSparse) {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    if (nRhs == 3) {
        if (!isThresholdCandidate(argIn[1])) {
            Error(L"Nelson:lu:invalidOption", L"The output form must be 'matrix' or 'vector'.");
        }
        options.hasThreshold = true;
        options.thresholds = parseThreshold(argIn[1], nLhs);
        options.asVector = parseOutputForm(argIn[2]);
    } else if (nRhs == 2) {
        if (isSparse && isThresholdCandidate(argIn[1])) {
            options.hasThreshold = true;
            options.thresholds = parseThreshold(argIn[1], nLhs);
        } else {
            options.asVector = parseOutputForm(argIn[1]);
        }
    }
    return options;
}
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::luBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 3);
    nargoutcheck(nLhs, 0, 5);
    const ArrayOf& A = argIn[0];
    if (!A.is2D()) {
        Error(L"Nelson:linear_algebra:matrix2DExpected", L"%s expected.", L"Vector or matrix 2D");
    }
    LUOptions options = parseOptions(argIn, nLhs, A.isSparse());
    if (nLhs > 3 && !A.isSparse()) {
        Error(L"Nelson:lu:sparseOnlyOutputs", L"Four or five outputs need a sparse matrix.");
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return sparseLUFactorization<double>(A, nLhs, options);
        }
        return LUMatrixFactorizationDoubleReal(A, nLhs, options.asVector);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return sparseLUFactorization<doublecomplex>(A, nLhs, options);
        }
        return LUMatrixFactorizationDoubleComplex(A, nLhs, options.asVector);
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            return sparseLUFactorization<single>(A, nLhs, options);
        }
        return LUMatrixFactorizationSingleReal(A, nLhs, options.asVector);
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            return sparseLUFactorization<singlecomplex>(A, nLhs, options);
        }
        return LUMatrixFactorizationSingleComplex(A, nLhs, options.asVector);
    } break;
    default: {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    } break;
    }
    return {};
}
//=============================================================================
