//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cholBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
#include "CholeskyFactorization.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Dense>
#include <Eigen/SparseCholesky>
#include <algorithm>
#include <vector>
#include <cwctype>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static bool
isLeadingBlockPositiveDefinite(
    const Eigen::SparseMatrix<T, 0, signedIndexType>& matrix, signedIndexType order)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    SparseMatrix leading = matrix.topLeftCorner(order, order);
    Eigen::SimplicialLLT<SparseMatrix, Eigen::Upper, Eigen::NaturalOrdering<signedIndexType>>
        solver;
    solver.compute(leading);
    return solver.info() == Eigen::Success;
}
//=============================================================================
// first failing pivot of a matrix whose factorization failed: a leading block
// fails exactly when it holds that pivot, so a binary search finds it
template <class T>
static int
sparseCholeskyFailureIndex(const Eigen::SparseMatrix<T, 0, signedIndexType>& matrix)
{
    signedIndexType low = 1;
    signedIndexType high = matrix.rows();
    while (low < high) {
        signedIndexType middle = low + (high - low) / 2;
        if (isLeadingBlockPositiveDefinite<T>(matrix, middle)) {
            low = middle + 1;
        } else {
            high = middle;
        }
    }
    return static_cast<int>(high);
}
//=============================================================================
// first rows of the upper factor, up to the failing pivot: [U11, U11' \ S12]
template <class T>
static Eigen::SparseMatrix<T, 0, signedIndexType>
leadingUpperRows(const Eigen::SparseMatrix<T, 0, signedIndexType>& source, int rows)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const signedIndexType order = source.cols();
    SparseMatrix R(rows, order);
    if (rows == 0) {
        return R;
    }
    SparseMatrix leading = source.topLeftCorner(rows, rows);
    Eigen::SimplicialLLT<SparseMatrix, Eigen::Upper, Eigen::NaturalOrdering<signedIndexType>>
        solver;
    solver.compute(leading);
    SparseMatrix upper = solver.matrixU();
    std::vector<Eigen::Triplet<T, signedIndexType>> entries;
    entries.reserve(static_cast<size_t>(upper.nonZeros()));
    for (signedIndexType col = 0; col < upper.outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(upper, col); it; ++it) {
            entries.emplace_back(it.row(), it.col(), it.value());
        }
    }
    // U11' \ S12 one column at a time, from its first nonzero row only
    std::vector<T> work(static_cast<size_t>(rows), T(0));
    for (signedIndexType col = rows; col < order; ++col) {
        signedIndexType firstRow = rows;
        for (typename SparseMatrix::InnerIterator it(source, col); it && it.row() < rows; ++it) {
            work[it.row()] = it.value();
            firstRow = std::min(firstRow, static_cast<signedIndexType>(it.row()));
        }
        for (signedIndexType i = firstRow; i < rows; ++i) {
            T value = work[i];
            typename Eigen::NumTraits<T>::Real diagonal = 0;
            for (typename SparseMatrix::InnerIterator it(upper, i); it; ++it) {
                if (it.row() == i) {
                    diagonal = Eigen::numext::real(it.value());
                } else if (it.row() >= firstRow) {
                    value -= Eigen::numext::conj(it.value()) * work[it.row()];
                }
            }
            work[i] = value / diagonal;
        }
        for (signedIndexType i = firstRow; i < rows; ++i) {
            if (work[i] != T(0)) {
                entries.emplace_back(i, col, work[i]);
                work[i] = T(0);
            }
        }
    }
    R.setFromTriplets(entries.begin(), entries.end());
    return R;
}
//=============================================================================
// real tridiagonal pattern with every sub and super diagonal entry stored
template <class T>
static bool
isFullTridiagonal(const Eigen::SparseMatrix<T, 0, signedIndexType>& matrix)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const signedIndexType order = matrix.cols();
    if (order < 2 || matrix.nonZeros() != 3 * order - 2) {
        return false;
    }
    for (signedIndexType col = 0; col < matrix.outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(matrix, col); it; ++it) {
            if (it.value() == T(0) || it.row() + 1 < col || it.row() > col + 1) {
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
template <class T>
static ArrayOfVector
sparseCholeskyFactorization(const ArrayOf& A, int nLhs, bool lowerTriangle)
{
    if (!A.isSquare()) {
        Error("Nelson:linear_algebra:squareMatrixExpected", "%s expected.", "Square matrix");
    }
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* spmat = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    // the lower triangle is read as the conjugate transpose of an upper one
    SparseMatrix source = lowerTriangle ? SparseMatrix(spmat->adjoint()) : *spmat;
    Eigen::SimplicialLLT<SparseMatrix, Eigen::Upper, Eigen::NaturalOrdering<signedIndexType>>
        solver;
    int nonReal = firstNonRealDiagonal(A);
    bool failed = nonReal != 0;
    if (!failed || nLhs > 1) {
        solver.compute(source);
        failed = failed || solver.info() != Eigen::Success;
    }
    // without the second output, the failing pivot is not needed
    if (failed && nLhs < 2) {
        Error(L"Nelson:chol:positiveFiniteMatrixExpected", L"%s expected.",
            L"Positive finite matrix");
    }
    int p = solver.info() == Eigen::Success ? 0 : sparseCholeskyFailureIndex<T>(source);
    if (nonReal != 0 && (p == 0 || nonReal < p)) {
        p = nonReal;
    }

    SparseMatrix R = p == 0 ? SparseMatrix(solver.matrixU()) : leadingUpperRows<T>(source, p - 1);
    // a failing real tridiagonal matrix keeps a square factor padded with zero rows
    if (p != 0 && !Eigen::NumTraits<T>::IsComplex && isFullTridiagonal<T>(*spmat)) {
        R.conservativeResize(source.rows(), source.cols());
    }
    if (lowerTriangle) {
        R = SparseMatrix(R.adjoint());
    }
    R.makeCompressed();

    ArrayOfVector retval;
    retval << makeSparseResult<T>(
        A.dataClass(), Dimensions(R.rows(), R.cols()), new SparseMatrix(R));
    if (nLhs > 1) {
        retval << ArrayOf::doubleScalar(p);
    }
    return retval;
}
//=============================================================================
// true for 'lower', false for 'upper'; any leading part of either word, any case
static bool
parseTriangleOption(const ArrayOf& arg)
{
    if (arg.isRowVectorCharacterArray() || arg.isScalarStringArray()) {
        std::wstring text = arg.asWideString();
        std::transform(text.begin(), text.end(), text.begin(), ::towlower);
        if (!text.empty() && std::wstring(L"lower").rfind(text, 0) == 0) {
            return true;
        }
        if (!text.empty() && std::wstring(L"upper").rfind(text, 0) == 0) {
            return false;
        }
    }
    Error(L"Nelson:chol:invalidOption", L"The triangle option must be 'upper' or 'lower'.");
    return false;
}
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::cholBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 2);
    bool lowerTriangle = argIn.size() == 2 && parseTriangleOption(argIn[1]);
    bool needToOverload;
    ArrayOf param1 = argIn[0];
    if (param1.isSparse()) {
        switch (param1.dataClass()) {
        case NLS_DOUBLE:
            return sparseCholeskyFactorization<double>(param1, nLhs, lowerTriangle);
        case NLS_DCOMPLEX:
            return sparseCholeskyFactorization<doublecomplex>(param1, nLhs, lowerTriangle);
        case NLS_SINGLE:
            return sparseCholeskyFactorization<single>(param1, nLhs, lowerTriangle);
        case NLS_SCOMPLEX:
            return sparseCholeskyFactorization<singlecomplex>(param1, nLhs, lowerTriangle);
        default:
            OverloadRequired("chol");
        }
    }
    int p = 0;
    bool nonRealDiagonal = false;
    ArrayOf res = CholeskyFactorization(param1, lowerTriangle, needToOverload, p, nonRealDiagonal);
    if (needToOverload) {
        OverloadRequired("chol");
    } else {
        if (p != 0 && nLhs < 2 && nonRealDiagonal) {
            Error(L"Nelson:chol:realDiagonalExpected",
                L"Positive definite matrix with a real diagonal expected.");
        }
        if (p != 0 && nLhs < 2) {
            Error(L"Nelson:chol:positiveFiniteMatrixExpected", L"%s expected.",
                L"Positive finite matrix");
        }
        retval << res;
        if (nLhs > 1) {
            retval << ArrayOf::doubleScalar(p);
        }
    }
    return retval;
}
//=============================================================================
