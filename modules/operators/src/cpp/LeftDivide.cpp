//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/IterativeLinearSolvers>
#include <Eigen/SparseCholesky>
#include <Eigen/SparseLU>
#include <Eigen/SparseQR>
#include <cmath>
#include <limits>
#include <string>
#include "LeftDivide.hpp"
#include "MatrixCheck.hpp"
#include "DotLeftDivide.hpp"
#include "LinearEquationSolver.hpp"
#include "LeastSquareSolver.hpp"
#include "SVDDecomposition.hpp"
#include "Warning.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "IntegerCompatibilityHelpers.hpp"
#include "SparseOperatorsHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>
arrayToDenseEigenMatrix(const ArrayOf& A)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> dense(A.rows(), A.columns());
    if (A.isSparse()) {
        dense.setZero();
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
            for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
                dense(it.row(), it.col()) = it.value();
            }
        }
    } else {
        const T* ptrA = static_cast<const T*>(A.dataPointer());
        for (indexType col = 0; col < A.columns(); ++col) {
            for (indexType row = 0; row < A.rows(); ++row) {
                dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col))
                    = ptrA[row + col * A.rows()];
            }
        }
    }
    return dense;
}
//=============================================================================
static NelsonType
emptyDivideResultClass(const ArrayOf& A, const ArrayOf& B)
{
    // the result is empty or all zeros: it is real
    if (A.isSingleClass() || B.isSingleClass()) {
        return NLS_SINGLE;
    }
    if (A.dataClass() == B.dataClass() && !A.isComplex()) {
        return A.dataClass();
    }
    return NLS_DOUBLE;
}
//=============================================================================
static void
promoteSparseDivideOperand(ArrayOf& value, NelsonType dataClass)
{
    if (value.dataClass() != dataClass) {
        value.promoteType(dataClass);
    }
}
//=============================================================================
static bool
isSparseDivideSingleClass(NelsonType dataClass)
{
    return dataClass == NLS_SINGLE || dataClass == NLS_SCOMPLEX;
}
//=============================================================================
static bool
isSparseDivideDoubleClass(NelsonType dataClass)
{
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX;
}
//=============================================================================
static ArrayOf
zeroLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    if (dimsC.elementCount() == 0) {
        ArrayOf res = ArrayOf::empty(dimsC);
        res.promoteType(dataClass);
        return res;
    }
    void* ptrC = ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount(), stringVector(), true);
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
template <class T>
static ArrayOf
zeroSparseLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(dimsC.rows(), dimsC.columns());
    return makeSparseOperatorResult<T>(dataClass, dimsC, sparseResult);
}
//=============================================================================
static ArrayOf
zeroSparseLeftDivideResult(NelsonType dataClass, const Dimensions& dimsC)
{
    switch (dataClass) {
    case NLS_SCOMPLEX:
        return zeroSparseLeftDivideResult<singlecomplex>(dataClass, dimsC);
    case NLS_DCOMPLEX:
        return zeroSparseLeftDivideResult<doublecomplex>(dataClass, dimsC);
    case NLS_SINGLE:
        return zeroSparseLeftDivideResult<single>(dataClass, dimsC);
    case NLS_DOUBLE:
    default:
        return zeroSparseLeftDivideResult<double>(NLS_DOUBLE, dimsC);
    }
}
//=============================================================================
template <class T>
static ArrayOf
sparseArrayToDenseArrayOf(NelsonType dataClass, const ArrayOf& A)
{
    Dimensions dims = A.dimensions();
    T* ptr = static_cast<T*>(
        ArrayOf::allocateArrayOf(dataClass, dims.elementCount(), stringVector(), true));
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    for (signedIndexType col = 0; col < sparseA->outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(*sparseA, col); it; ++it) {
            ptr[it.row() + it.col() * dims.rows()] = it.value();
        }
    }
    return ArrayOf(dataClass, dims, ptr, false);
}
//=============================================================================
template <class T>
static ArrayOf
eigenDenseMatrixToArrayOf(
    NelsonType dataClass, const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& dense)
{
    Dimensions dims(dense.rows(), dense.cols());
    T* ptr = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, dims.elementCount()));
    for (indexType col = 0; col < dims.columns(); ++col) {
        for (indexType row = 0; row < dims.rows(); ++row) {
            ptr[row + col * dims.rows()]
                = dense(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
        }
    }
    return ArrayOf(dataClass, dims, ptr, false);
}
//=============================================================================
template <class T>
static ArrayOf
eigenDenseMatrixToSparseArrayOf(
    NelsonType dataClass, const Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>& dense)
{
    auto* sparseResult = new NelsonSparseMatrix<T>(dense.sparseView());
    return makeSparseOperatorResult<T>(
        dataClass, Dimensions(dense.rows(), dense.cols()), sparseResult);
}
//=============================================================================
template <class T>
static bool
isSparseDiagonal(const NelsonSparseMatrix<T>& A)
{
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            if (it.row() != it.col() && it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseHermitian(const NelsonSparseMatrix<T>& A)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    RealScalar scale = RealScalar(1);
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            scale = std::max(scale, static_cast<RealScalar>(std::abs(it.value())));
        }
    }
    const RealScalar tolerance = Eigen::NumTraits<RealScalar>::dummy_precision() * scale;
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            T transposed = Eigen::numext::conj(A.coeff(it.col(), it.row()));
            if (std::abs(it.value() - transposed) > tolerance) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseZero(const NelsonSparseMatrix<T>& A)
{
    for (signedIndexType col = 0; col < A.outerSize(); ++col) {
        for (typename NelsonSparseMatrix<T>::InnerIterator it(A, col); it; ++it) {
            if (it.value() != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static bool
isSparseDiagonalSingular(const NelsonSparseMatrix<T>& A)
{
    if (!isSparseDiagonal<T>(A)) {
        return false;
    }
    for (signedIndexType k = 0; k < A.rows(); ++k) {
        if (A.coeff(k, k) == T(0)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
template <class T>
static bool
isDenseRegularDiagonal(const ArrayOf& A)
{
    if (A.rows() != A.columns()) {
        return false;
    }
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    for (indexType col = 0; col < A.columns(); ++col) {
        for (indexType row = 0; row < A.rows(); ++row) {
            const T value = ptrA[row + col * A.rows()];
            if (row == col) {
                if (value == T(0)) {
                    return false;
                }
            } else if (value != T(0)) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static ArrayOf
denseRegularDiagonalLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    const T* ptrA = static_cast<const T*>(A.dataPointer());
    const T* ptrB = static_cast<const T*>(B.dataPointer());
    Dimensions dimsC(A.columns(), B.columns());
    T* ptrC = static_cast<T*>(ArrayOf::allocateArrayOf(dataClass, dimsC.elementCount()));
    for (indexType col = 0; col < B.columns(); ++col) {
        for (indexType row = 0; row < A.columns(); ++row) {
            ptrC[row + col * dimsC.rows()]
                = ptrB[row + col * B.rows()] / ptrA[row + row * A.rows()];
        }
    }
    return ArrayOf(dataClass, dimsC, ptrC, false);
}
//=============================================================================
static ArrayOf
denseRegularDiagonalLeftDivide(const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    switch (A.dataClass()) {
    case NLS_DOUBLE:
        if (isDenseRegularDiagonal<double>(A)) {
            solved = true;
            return denseRegularDiagonalLeftDivide<double>(NLS_DOUBLE, A, B);
        }
        break;
    case NLS_SINGLE:
        if (isDenseRegularDiagonal<single>(A)) {
            solved = true;
            return denseRegularDiagonalLeftDivide<single>(NLS_SINGLE, A, B);
        }
        break;
    case NLS_DCOMPLEX:
        if (isDenseRegularDiagonal<doublecomplex>(A)) {
            solved = true;
            return denseRegularDiagonalLeftDivide<doublecomplex>(NLS_DCOMPLEX, A, B);
        }
        break;
    case NLS_SCOMPLEX:
        if (isDenseRegularDiagonal<singlecomplex>(A)) {
            solved = true;
            return denseRegularDiagonalLeftDivide<singlecomplex>(NLS_SCOMPLEX, A, B);
        }
        break;
    default:
        break;
    }
    return {};
}
//=============================================================================
static void
warnSparseRankDeficient(indexType rows, indexType cols, signedIndexType rank)
{
    signedIndexType expectedRank = static_cast<signedIndexType>(rows < cols ? rows : cols);
    if (rank < expectedRank) {
        if (rows == cols) {
            Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
            return;
        }
        RaiseWarning("Nelson:rankDeficientMatrix",
            resolveMessageTemplate("Nelson:operators:matrixRankDeficientMachine",
                "Matrix is rank deficient to machine precision:")
                + " RANK = " + std::to_string(rank));
    }
}
//=============================================================================
static double
zeroPivotQuotient(double value)
{
    if (value == 0.) {
        return std::numeric_limits<double>::quiet_NaN();
    }
    return std::copysign(std::numeric_limits<double>::infinity(), value);
}
//=============================================================================
static single
zeroPivotQuotient(single value)
{
    if (value == single(0)) {
        return std::numeric_limits<single>::quiet_NaN();
    }
    return std::copysign(std::numeric_limits<single>::infinity(), value);
}
//=============================================================================
static doublecomplex
zeroPivotQuotient(const doublecomplex& value)
{
    const double inf = std::numeric_limits<double>::infinity();
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const double re = value.real();
    const double im = value.imag();
    if (re == 0. && im == 0.) {
        return { nan, 0. };
    }
    return { re == 0. ? 0. : std::copysign(inf, re), im == 0. ? 0. : std::copysign(inf, im) };
}
//=============================================================================
static singlecomplex
zeroPivotQuotient(const singlecomplex& value)
{
    const single inf = std::numeric_limits<single>::infinity();
    const single nan = std::numeric_limits<single>::quiet_NaN();
    const single re = value.real();
    const single im = value.imag();
    if (re == single(0) && im == single(0)) {
        return { nan, single(0) };
    }
    return { re == single(0) ? single(0) : std::copysign(inf, re),
        im == single(0) ? single(0) : std::copysign(inf, im) };
}
//=============================================================================
template <class T>
static ArrayOf
sparseSingularDiagonalLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    for (signedIndexType row = 0; row < rhs.rows(); ++row) {
        T pivot = sparseA->coeff(row, row);
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            if (pivot == T(0)) {
                result(row, col) = zeroPivotQuotient(rhs(row, col));
            } else {
                result(row, col) = rhs(row, col) / pivot;
            }
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseRegularDiagonalLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    for (signedIndexType row = 0; row < rhs.rows(); ++row) {
        T pivot = sparseA->coeff(row, row);
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            result(row, col) = rhs(row, col) / pivot;
        }
    }
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseZeroSquareLeftDivide(NelsonType dataClass, const ArrayOf& B)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = arrayToDenseEigenMatrix<T>(B);
    for (signedIndexType col = 0; col < result.cols(); ++col) {
        for (signedIndexType row = 0; row < result.rows(); ++row) {
            result(row, col) = zeroPivotQuotient(result(row, col));
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseSingularSquareLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B)
{
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> lu = arrayToDenseEigenMatrix<T>(A);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    const signedIndexType n = lu.rows();
    std::vector<signedIndexType> pivots(static_cast<size_t>(n));
    for (signedIndexType k = 0; k < n; ++k) {
        signedIndexType pivot = k;
        auto best = std::abs(lu(k, k));
        for (signedIndexType row = k + 1; row < n; ++row) {
            auto candidate = std::abs(lu(row, k));
            if (candidate > best) {
                best = candidate;
                pivot = row;
            }
        }
        pivots[static_cast<size_t>(k)] = pivot;
        if (pivot != k) {
            lu.row(k).swap(lu.row(pivot));
        }
        if (lu(k, k) == T(0)) {
            continue;
        }
        for (signedIndexType row = k + 1; row < n; ++row) {
            lu(row, k) /= lu(k, k);
            for (signedIndexType col = k + 1; col < n; ++col) {
                lu(row, col) -= lu(row, k) * lu(k, col);
            }
        }
    }

    for (signedIndexType k = 0; k < n; ++k) {
        signedIndexType pivot = pivots[static_cast<size_t>(k)];
        if (pivot != k) {
            rhs.row(k).swap(rhs.row(pivot));
        }
    }
    for (signedIndexType k = 0; k < n; ++k) {
        for (signedIndexType row = k + 1; row < n; ++row) {
            rhs.row(row) -= lu(row, k) * rhs.row(k);
        }
    }

    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result(rhs.rows(), rhs.cols());
    result.setZero();
    for (signedIndexType k = n - 1; k >= 0; --k) {
        for (signedIndexType col = 0; col < rhs.cols(); ++col) {
            T value = rhs(k, col);
            for (signedIndexType row = k + 1; row < n; ++row) {
                value -= lu(k, row) * result(row, col);
            }
            result(k, col) = value / lu(k, k);
        }
    }
    Warning("Nelson:singularMatrix", "Matrix is singular to working precision.");
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseSimplicialLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    if (isSparseDiagonal<T>(*sparseA) || !isSparseHermitian<T>(*sparseA)) {
        return {};
    }

    Eigen::SimplicialLLT<NelsonSparseMatrix<T>, Eigen::Lower, Eigen::AMDOrdering<signedIndexType>>
        lltSolver;
    lltSolver.compute(*sparseA);
    if (lltSolver.info() == Eigen::Success) {
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = lltSolver.solve(rhs);
        if (lltSolver.info() == Eigen::Success) {
            solved = true;
            if (B.isSparse()) {
                return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
            }
            return eigenDenseMatrixToArrayOf<T>(dataClass, result);
        }
    }

    Eigen::SimplicialLDLT<NelsonSparseMatrix<T>, Eigen::Lower, Eigen::AMDOrdering<signedIndexType>>
        ldltSolver;
    ldltSolver.compute(*sparseA);
    if (ldltSolver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = ldltSolver.solve(rhs);
    if (ldltSolver.info() != Eigen::Success) {
        return {};
    }
    solved = true;
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseLeastSquaresIterativeLeftDivide(
    NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    using RealScalar = typename Eigen::NumTraits<T>::Real;
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::LeastSquaresConjugateGradient<NelsonSparseMatrix<T>> solver;
    RealScalar tolerance
        = sizeof(RealScalar) == sizeof(single) ? RealScalar(1e-6f) : RealScalar(1e-12);
    solver.setTolerance(tolerance);
    solver.setMaxIterations(static_cast<int>(std::max<indexType>(20, A.columns() * 4)));
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> residual = (*sparseA) * result - rhs;
    RealScalar rhsNorm = rhs.norm();
    if (rhsNorm == RealScalar(0)) {
        rhsNorm = RealScalar(1);
    }
    RealScalar relativeResidual = residual.norm() / rhsNorm;
    RealScalar acceptedResidual
        = sizeof(RealScalar) == sizeof(single) ? RealScalar(1e-4f) : RealScalar(1e-7);
    if (!std::isfinite(static_cast<double>(relativeResidual))
        || relativeResidual > acceptedResidual) {
        return {};
    }
    solved = true;
    if (B.isSparse()) {
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
static bool
preferIterativeRectangularSparsePath(const ArrayOf& A, const ArrayOf& B)
{
    if (A.rows() == A.columns()) {
        return false;
    }
    if (B.columns() > 4) {
        return false;
    }
    indexType largeDimension = std::max(A.rows(), A.columns());
    indexType smallDimension = std::min(A.rows(), A.columns());
    return largeDimension >= 1200 && smallDimension >= 400;
}
//=============================================================================
template <class T>
static ArrayOf
sparseQrLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::SparseQR<NelsonSparseMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    if (B.isSparse()) {
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
        Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
        if (solver.info() != Eigen::Success) {
            return {};
        }
        warnSparseRankDeficient(A.rows(), A.columns(), solver.rank());
        solved = true;
        return eigenDenseMatrixToSparseArrayOf<T>(dataClass, result);
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    warnSparseRankDeficient(A.rows(), A.columns(), solver.rank());
    solved = true;
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseLuLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& solved)
{
    solved = false;
    const auto* sparseA = getSparseMatrixPointer<T>(A);
    Eigen::SparseLU<NelsonSparseMatrix<T>, Eigen::COLAMDOrdering<signedIndexType>> solver;
    solver.compute(*sparseA);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    if (B.isSparse()) {
        const auto* sparseB = getSparseMatrixPointer<T>(B);
        auto* sparseResult = new NelsonSparseMatrix<T>(solver.solve(*sparseB));
        if (solver.info() != Eigen::Success) {
            delete sparseResult;
            return {};
        }
        solved = true;
        return makeSparseOperatorResult<T>(
            dataClass, Dimensions(A.columns(), B.columns()), sparseResult);
    }
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> rhs = arrayToDenseEigenMatrix<T>(B);
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic> result = solver.solve(rhs);
    if (solver.info() != Eigen::Success) {
        return {};
    }
    solved = true;
    return eigenDenseMatrixToArrayOf<T>(dataClass, result);
}
//=============================================================================
template <class T>
static ArrayOf
sparseLeftDivide(NelsonType dataClass, const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    needToOverload = false;
    if (A.dataClass() != B.dataClass()) {
        needToOverload = true;
        return {};
    }
    if (!A.isSparse() && B.isSparse()) {
        ArrayOf denseB = sparseArrayToDenseArrayOf<T>(dataClass, B);
        return LeftDivide(A, denseB, needToOverload);
    }
    if (!A.isSparse()) {
        needToOverload = true;
        return {};
    }
    if (A.rows() != B.rows()) {
        Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
            L"Requested divide operation requires arguments to have correct dimensions.");
    }

    if (A.rows() == A.columns()) {
        const auto* sparseA = getSparseMatrixPointer<T>(A);
        if (isSparseZero<T>(*sparseA)) {
            return sparseZeroSquareLeftDivide<T>(dataClass, B);
        }
        if (isSparseDiagonalSingular<T>(*sparseA)) {
            return sparseSingularDiagonalLeftDivide<T>(dataClass, A, B);
        }
        if (isSparseDiagonal<T>(*sparseA)) {
            return sparseRegularDiagonalLeftDivide<T>(dataClass, A, B);
        }
        bool solved = false;
        ArrayOf result = sparseSimplicialLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        result = sparseLuLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        return sparseSingularSquareLeftDivide<T>(dataClass, A, B);
    } else {
        bool solved = false;
        if (preferIterativeRectangularSparsePath(A, B)) {
            ArrayOf result = sparseLeastSquaresIterativeLeftDivide<T>(dataClass, A, B, solved);
            if (solved) {
                return result;
            }
        }
        ArrayOf result = sparseQrLeftDivide<T>(dataClass, A, B, solved);
        if (solved) {
            return result;
        }
        if (!preferIterativeRectangularSparsePath(A, B)) {
            result = sparseLeastSquaresIterativeLeftDivide<T>(dataClass, A, B, solved);
            if (solved) {
                return result;
            }
        }
    }
    bool solved = false;
    ArrayOf result = sparseQrLeftDivide<T>(dataClass, A, B, solved);
    if (solved) {
        return result;
    }
    Error(L"Nelson:operators:sparseSolverFailed", L"Sparse linear equation solver failed.");
    return {};
}
//=============================================================================
static ArrayOf
sparseLeftDivide(const ArrayOf& A, const ArrayOf& B, bool& needToOverload)
{
    switch (A.dataClass()) {
    case NLS_SINGLE:
        return sparseLeftDivide<single>(NLS_SINGLE, A, B, needToOverload);
    case NLS_SCOMPLEX:
        return sparseLeftDivide<singlecomplex>(NLS_SCOMPLEX, A, B, needToOverload);
    case NLS_DOUBLE:
        return sparseLeftDivide<double>(NLS_DOUBLE, A, B, needToOverload);
    case NLS_DCOMPLEX:
        return sparseLeftDivide<doublecomplex>(NLS_DCOMPLEX, A, B, needToOverload);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
ArrayOf
LeftDivide(ArrayOf A, ArrayOf B, bool& needToOverload)
{
    needToOverload = false;
    if (A.isScalar()) {
        return DotLeftDivide(A, B, needToOverload);
    }
    if (A.isEmpty() || B.isEmpty()) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        dimsA.simplify();
        dimsB.simplify();
        if (dimsA.length() > 2 || dimsB.length() > 2) {
            Error(L"Nelson:expected2D", L"%s must be 2-D.", L"Input arguments");
        }
        if (dimsA[0] != dimsB[0]) {
            Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
                L"Requested divide operation requires arguments to have correct dimensions.");
        }
        Dimensions dimsC(dimsA[1], dimsB[1]);
        NelsonType dataClass = emptyDivideResultClass(A, B);
        if (A.isSparse() && B.isSparse()) {
            return zeroSparseLeftDivideResult(dataClass, dimsC);
        }
        return zeroLeftDivideResult(dataClass, dimsC);
    }

    if (A.isSparse() || B.isSparse()) {
        if (A.dataClass() != B.dataClass()) {
            bool isComplex = A.isComplex() || B.isComplex();
            NelsonType dataClassA = A.dataClass();
            NelsonType dataClassB = B.dataClass();
            if ((isSparseDivideDoubleClass(dataClassA) && isSparseDivideSingleClass(dataClassB))
                || (isSparseDivideSingleClass(dataClassA)
                    && isSparseDivideDoubleClass(dataClassB))) {
                if (isComplex) {
                    promoteSparseDivideOperand(A, NLS_SCOMPLEX);
                    promoteSparseDivideOperand(B, NLS_SCOMPLEX);
                } else {
                    promoteSparseDivideOperand(A, NLS_SINGLE);
                    promoteSparseDivideOperand(B, NLS_SINGLE);
                }
                return sparseLeftDivide(A, B, needToOverload);
            }
            if (isSparseDivideDoubleClass(dataClassA) && isSparseDivideDoubleClass(dataClassB)
                && isComplex) {
                promoteSparseDivideOperand(A, NLS_DCOMPLEX);
                promoteSparseDivideOperand(B, NLS_DCOMPLEX);
                return sparseLeftDivide(A, B, needToOverload);
            }
            if (isSparseDivideSingleClass(dataClassA) && isSparseDivideSingleClass(dataClassB)
                && isComplex) {
                promoteSparseDivideOperand(A, NLS_SCOMPLEX);
                promoteSparseDivideOperand(B, NLS_SCOMPLEX);
                return sparseLeftDivide(A, B, needToOverload);
            }
        }
        return sparseLeftDivide(A, B, needToOverload);
    }

    if (A.dimensionLength(0) != B.dimensionLength(0)) {
        Error(L"Nelson:dimagree:requestedDivideOperationRequiresArgumentsCorrect",
            L"Requested divide operation requires arguments to have correct dimensions.");
    }

    if (A.dataClass() != B.dataClass()) {
        bool isComplex = A.isComplex() || B.isComplex();
        if ((A.isDoubleClass() && B.isSingleClass()) || (A.isSingleClass() && B.isDoubleClass())) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            if (isComplex) {
                AA.promoteType(NLS_SCOMPLEX);
                BB.promoteType(NLS_SCOMPLEX);
            } else {
                AA.promoteType(NLS_SINGLE);
                BB.promoteType(NLS_SINGLE);
            }
            return LeftDivide(AA, BB, needToOverload);
        }
        if (A.isDoubleType() && B.isDoubleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_DCOMPLEX);
            BB.promoteType(NLS_DCOMPLEX);
            return LeftDivide(AA, BB, needToOverload);
        }
        if (A.isSingleType() && B.isSingleType() && isComplex) {
            ArrayOf AA = A;
            ArrayOf BB = B;
            AA.promoteType(NLS_SCOMPLEX);
            BB.promoteType(NLS_SCOMPLEX);
            return LeftDivide(AA, BB, needToOverload);
        }

        if (A.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(B);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (B.dataClass() == NLS_CHAR) {
                ArrayOf BB = B;
                BB.promoteType(A.dataClass());
                return LeftDivide(A, BB, needToOverload);
            }
            if (B.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf AA = A;
            AA.promoteType(B.dataClass());
            ArrayOf res = LeftDivide(AA, B, needToOverload);
            if (!needToOverload) {
                res.promoteType(A.dataClass());
            }
            return res;
        } else if (B.isIntegerType()) {
            bool isCompatible = isScalarDoubleOrChar(A);
            if (!isCompatible) {
                Error(L"Nelson:operators:integerClassMismatch",
                    L"Integers can only be combined with integers of the same class, or scalar "
                    L"doubles.");
            }
            if (A.dataClass() == NLS_CHAR) {
                ArrayOf AA = A;
                AA.promoteType(B.dataClass());
                return LeftDivide(AA, B, needToOverload);
            }
            if (A.isComplex()) {
                Error(L"Nelson:operators:complexIntegerNotAllowed",
                    L"Complex integer not allowed for arithmetic operator *");
            }
            ArrayOf BB = B;
            BB.promoteType(A.dataClass());
            ArrayOf res = LeftDivide(A, BB, needToOverload);
            if (!needToOverload) {
                res.promoteType(B.dataClass());
            }
            return res;
        }
    }

    bool diagonalSolved = false;
    ArrayOf diagonalResult = denseRegularDiagonalLeftDivide(A, B, diagonalSolved);
    if (diagonalSolved) {
        return diagonalResult;
    }

    std::wstring warningId;
    std::string warningMessage;
    ArrayOf res;

    bool isSquare = A.dimensionLength(0) == A.dimensionLength(1);

    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        if (isSquare) {
            res = solveLinearEquationDouble(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareDouble(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionDouble(A, B);
        }
    } break;
    case NLS_SINGLE: {
        if (isSquare) {
            res = solveLinearEquationSingle(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareSingle(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionSingle(A, B);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationDoubleComplex(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareDoubleComplex(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionDoubleComplex(A, B);
        }
    } break;
    case NLS_SCOMPLEX: {
        if (isSquare) {
            res = solveLinearEquationSingleComplex(A, B, warningId, warningMessage);
        } else {
            res = solveLeastSquareSingleComplex(A, B, warningId, warningMessage);
        }
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX
            || warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            res = solveSVDDecompositionSingleComplex(A, B);
        }
    } break;

    default: {
        needToOverload = true;
    } break;
    }
    if (!warningMessage.empty()) {
        std::string warningIdNarrow;
        if (warningId == WARNING_RANK_DEFICIENT_MATRIX) {
            warningIdNarrow = "Nelson:rankDeficientMatrix";
        } else if (warningId == WARNING_NEARLY_SINGULAR_MATRIX) {
            warningIdNarrow = "Nelson:nearlySingularMatrix";
        }
        RaiseWarning(warningIdNarrow, warningMessage);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
