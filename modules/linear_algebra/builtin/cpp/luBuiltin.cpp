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
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static void
swapSparseRows(Eigen::SparseMatrix<T, Eigen::RowMajor, signedIndexType>& matrix,
    signedIndexType firstRow, signedIndexType secondRow);
//=============================================================================
template <class T>
static ArrayOfVector
sparseLUFactorization(const ArrayOf& A, int nLhs)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using RowSparseMatrix = Eigen::SparseMatrix<T, Eigen::RowMajor, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    const auto* spmat = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    signedIndexType rows = static_cast<signedIndexType>(A.rows());
    signedIndexType cols = static_cast<signedIndexType>(A.columns());
    signedIndexType order = std::min(rows, cols);
    RowSparseMatrix working(*spmat);
    RowSparseMatrix lower(rows, order);
    std::vector<signedIndexType> rowPermutation(rows);
    for (signedIndexType row = 0; row < rows; ++row) {
        rowPermutation[row] = row;
    }

    for (signedIndexType k = 0; k < order; ++k) {
        signedIndexType pivotRow = k;
        double pivotMagnitude = std::abs(working.coeff(k, k));
        for (signedIndexType row = k + 1; row < rows; ++row) {
            double candidateMagnitude = std::abs(working.coeff(row, k));
            if (candidateMagnitude > pivotMagnitude) {
                pivotMagnitude = candidateMagnitude;
                pivotRow = row;
            }
        }
        if (pivotMagnitude == 0) {
            Error(L"Nelson:linear_algebra:factorizationFailed", L"%s failed.",
                L"Matrix factorization");
        }
        if (pivotRow != k) {
            swapSparseRows<T>(working, k, pivotRow);
            for (signedIndexType col = 0; col < k; ++col) {
                T value = lower.coeff(k, col);
                lower.coeffRef(k, col) = lower.coeff(pivotRow, col);
                lower.coeffRef(pivotRow, col) = value;
            }
            std::swap(rowPermutation[k], rowPermutation[pivotRow]);
        }
        T pivot = working.coeff(k, k);
        lower.coeffRef(k, k) = T(1);
        std::vector<std::pair<signedIndexType, T>> pivotValues;
        for (typename RowSparseMatrix::InnerIterator it(working, k); it; ++it) {
            if (it.col() > k && it.value() != T(0)) {
                pivotValues.emplace_back(it.col(), it.value());
            }
        }
        for (signedIndexType row = k + 1; row < rows; ++row) {
            T columnValue = working.coeff(row, k);
            if (columnValue == T(0)) {
                continue;
            }
            T factor = columnValue / pivot;
            lower.coeffRef(row, k) = factor;
            working.coeffRef(row, k) = T(0);
            for (const auto& item : pivotValues) {
                signedIndexType col = item.first;
                T value = working.coeff(row, col) - factor * item.second;
                working.coeffRef(row, col) = value;
            }
        }
        working.prune(T(0));
    }

    lower.makeCompressed();

    if (nLhs <= 1) {
        std::vector<Triplet> compactTriplets;
        for (signedIndexType row = 0; row < rows; ++row) {
            for (typename RowSparseMatrix::InnerIterator it(lower, row); it; ++it) {
                if (it.col() < row && it.value() != T(0)) {
                    compactTriplets.emplace_back(row, it.col(), it.value());
                }
            }
        }
        for (signedIndexType row = 0; row < order; ++row) {
            for (typename RowSparseMatrix::InnerIterator it(working, row); it; ++it) {
                if (it.col() >= row && it.value() != T(0)) {
                    compactTriplets.emplace_back(row, it.col(), it.value());
                }
            }
        }
        auto* compact = new SparseMatrix(rows, cols);
        compact->setFromTriplets(compactTriplets.begin(), compactTriplets.end());
        compact->makeCompressed();
        ArrayOfVector retval;
        retval << makeSparseResult<T>(A.dataClass(), Dimensions(rows, cols), compact);
        return retval;
    }

    std::vector<Triplet> uTriplets;
    for (signedIndexType row = 0; row < order; ++row) {
        for (typename RowSparseMatrix::InnerIterator it(working, row); it; ++it) {
            if (it.col() >= row && it.value() != T(0)) {
                uTriplets.emplace_back(row, it.col(), it.value());
            }
        }
    }
    auto* upper = new SparseMatrix(order, cols);
    upper->setFromTriplets(uTriplets.begin(), uTriplets.end());
    upper->makeCompressed();

    ArrayOfVector retval;
    std::vector<Triplet> lTriplets;
    if (nLhs == 2) {
        for (signedIndexType row = 0; row < rows; ++row) {
            for (typename RowSparseMatrix::InnerIterator it(lower, row); it; ++it) {
                if (it.value() != T(0)) {
                    lTriplets.emplace_back(rowPermutation[row], it.col(), it.value());
                }
            }
        }
    } else {
        for (signedIndexType row = 0; row < rows; ++row) {
            for (typename RowSparseMatrix::InnerIterator it(lower, row); it; ++it) {
                if (it.value() != T(0)) {
                    lTriplets.emplace_back(row, it.col(), it.value());
                }
            }
        }
    }
    auto* finalLower = new SparseMatrix(rows, order);
    finalLower->setFromTriplets(lTriplets.begin(), lTriplets.end());
    finalLower->makeCompressed();

    retval << makeSparseResult<T>(A.dataClass(), Dimensions(rows, order), finalLower);
    retval << makeSparseResult<T>(A.dataClass(), Dimensions(order, cols), upper);
    if (nLhs > 2) {
        std::vector<Triplet> pTriplets;
        pTriplets.reserve(rows);
        for (signedIndexType row = 0; row < rows; ++row) {
            pTriplets.emplace_back(row, rowPermutation[row], T(1));
        }
        auto* P = new SparseMatrix(rows, rows);
        P->setFromTriplets(pTriplets.begin(), pTriplets.end());
        P->makeCompressed();
        retval << makeSparseResult<T>(A.dataClass(), Dimensions(rows, rows), P);
    }
    return retval;
}
//=============================================================================
template <class T>
static void
swapSparseRows(Eigen::SparseMatrix<T, Eigen::RowMajor, signedIndexType>& matrix,
    signedIndexType firstRow, signedIndexType secondRow)
{
    using RowSparseMatrix = Eigen::SparseMatrix<T, Eigen::RowMajor, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<Triplet> triplets;
    triplets.reserve(matrix.nonZeros());
    for (signedIndexType row = 0; row < matrix.rows(); ++row) {
        signedIndexType targetRow = row;
        if (row == firstRow) {
            targetRow = secondRow;
        } else if (row == secondRow) {
            targetRow = firstRow;
        }
        for (typename RowSparseMatrix::InnerIterator it(matrix, row); it; ++it) {
            if (it.value() != T(0)) {
                triplets.emplace_back(targetRow, it.col(), it.value());
            }
        }
    }
    matrix.setZero();
    matrix.resize(matrix.rows(), matrix.cols());
    matrix.setFromTriplets(triplets.begin(), triplets.end());
    matrix.makeCompressed();
}
//=============================================================================
ArrayOfVector
Nelson::LinearAlgebraGateway::luBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 3);

    ArrayOf A = argIn[0];
    if (!A.is2D()) {
        Error(L"Nelson:linear_algebra:matrix2DExpected", L"%s expected.", L"Vector or matrix 2D");
    }
    switch (A.dataClass()) {
    case NLS_DOUBLE: {
        if (A.isSparse()) {
            return sparseLUFactorization<double>(A, nLhs);
        }
        retval = LUMatrixFactorizationDoubleReal(A, nLhs);
    } break;
    case NLS_DCOMPLEX: {
        if (A.isSparse()) {
            return sparseLUFactorization<doublecomplex>(A, nLhs);
        }
        retval = LUMatrixFactorizationDoubleComplex(A, nLhs);
    } break;
    case NLS_SINGLE: {
        if (A.isSparse()) {
            return sparseLUFactorization<single>(A, nLhs);
        }
        retval = LUMatrixFactorizationSingleReal(A, nLhs);
    } break;
    case NLS_SCOMPLEX: {
        if (A.isSparse()) {
            return sparseLUFactorization<singlecomplex>(A, nLhs);
        }
        retval = LUMatrixFactorizationSingleComplex(A, nLhs);
    } break;
    default: {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"single or double");
    } break;
    }
    return retval;
}
//=============================================================================
