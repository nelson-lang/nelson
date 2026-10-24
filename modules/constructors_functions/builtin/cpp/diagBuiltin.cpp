//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "diagBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SparseArrayOf.hpp"
#include "Decomplexify.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <cmath>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
template <class T>
static ArrayOf
sparseDiagResult(NelsonType dataClass, const Dimensions& dimensions,
    Eigen::SparseMatrix<T, 0, signedIndexType>* sparseMatrix)
{
    return makeSparseArrayOfFromEigen<T>(dataClass, dimensions, sparseMatrix);
}
//=============================================================================
template <class T>
static ArrayOf
sparseMatrixDiagonal(const ArrayOf& a, int64 diagonalOrder)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    indexType rows = a.rows();
    indexType cols = a.columns();
    indexType len = 0;
    if (diagonalOrder >= 0) {
        indexType offset = static_cast<indexType>(diagonalOrder);
        len = (offset < cols) ? std::min(rows, cols - offset) : 0;
    } else {
        indexType offset = static_cast<indexType>(-diagonalOrder);
        len = (offset < rows) ? std::min(rows - offset, cols) : 0;
    }

    auto* spmatOut = new SparseMatrix(len, 1);
    std::vector<Triplet> triplets;
    triplets.reserve(static_cast<size_t>(len));
    const auto* spmatIn = static_cast<const SparseMatrix*>(a.sparseDataPointer());
    for (indexType k = 0; k < len; ++k) {
        indexType row = (diagonalOrder >= 0) ? k : k - diagonalOrder;
        indexType col = (diagonalOrder >= 0) ? k + diagonalOrder : k;
        T value
            = spmatIn->coeff(static_cast<signedIndexType>(row), static_cast<signedIndexType>(col));
        if (value != T(0)) {
            triplets.emplace_back(static_cast<signedIndexType>(k), 0, value);
        }
    }
    spmatOut->setFromTriplets(triplets.begin(), triplets.end());
    return sparseDiagResult<T>(a.dataClass(), Dimensions(len, 1), spmatOut);
}
//=============================================================================
template <class T>
static ArrayOf
sparseVectorDiagonal(const ArrayOf& a, int64 diagonalOrder)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    indexType len = a.elementCount();
    indexType offset = static_cast<indexType>(diagonalOrder < 0 ? -diagonalOrder : diagonalOrder);
    indexType size = len + offset;
    auto* spmatOut = new SparseMatrix(size, size);
    std::vector<Triplet> triplets;
    triplets.reserve(static_cast<size_t>(a.nnz()));
    const auto* spmatIn = static_cast<const SparseMatrix*>(a.sparseDataPointer());
    for (signedIndexType col = 0; col < spmatIn->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*spmatIn, col); it; ++it) {
            indexType ndx = static_cast<indexType>(it.row() + it.col() * a.rows());
            indexType rowOut = (diagonalOrder >= 0) ? ndx : ndx + offset;
            indexType colOut = (diagonalOrder >= 0) ? ndx + offset : ndx;
            triplets.emplace_back(static_cast<signedIndexType>(rowOut),
                static_cast<signedIndexType>(colOut), it.value());
        }
    }
    spmatOut->setFromTriplets(triplets.begin(), triplets.end());
    return sparseDiagResult<T>(a.dataClass(), Dimensions(size, size), spmatOut);
}
//=============================================================================
static ArrayOf
sparseDiagonal(const ArrayOf& a, int64 diagonalOrder)
{
    bool isVector = (a.dimensionLength(1) == 1) || (a.dimensionLength(0) == 1);
    switch (a.dataClass()) {
    case NLS_DOUBLE:
        return isVector ? sparseVectorDiagonal<double>(a, diagonalOrder)
                        : sparseMatrixDiagonal<double>(a, diagonalOrder);
    case NLS_DCOMPLEX:
        return isVector ? sparseVectorDiagonal<doublecomplex>(a, diagonalOrder)
                        : sparseMatrixDiagonal<doublecomplex>(a, diagonalOrder);
    case NLS_LOGICAL:
        return isVector ? sparseVectorDiagonal<logical>(a, diagonalOrder)
                        : sparseMatrixDiagonal<logical>(a, diagonalOrder);
    default:
        Error(L"Nelson:constructors_functions:unsupportedTypeSparseMatrix",
            L"Unsupported type in sparse matrix.");
    }
    return {};
}
//=============================================================================
ArrayOfVector
Nelson::ConstructorsGateway::diagBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);
    ArrayOf b;
    int64* dp;
    int64 diagonalOrder;
    if (argIn.size() == 1) {
        diagonalOrder = 0;
    } else {
        b = argIn[1];
        if (!b.isScalar()
            || !(b.isNumeric() || b.dataClass() == NLS_LOGICAL || b.isCharacterArray())
            || b.isComplex()) {
            Error(L"Nelson:constructors_functions:secondArgumentScalar",
                L"Second argument must be a scalar.");
        }
        ArrayOf orderValue(b);
        orderValue.promoteType(NLS_DOUBLE);
        double orderAsDouble = static_cast<const double*>(orderValue.dataPointer())[0];
        if (!std::isfinite(orderAsDouble) || std::floor(orderAsDouble) != orderAsDouble) {
            Error(L"Nelson:constructors_functions:secondArgumentIntegerScalar",
                L"Second argument must be an integer scalar.");
        }
        b.promoteType(NLS_INT64);
        dp = (int64*)b.dataPointer();
        diagonalOrder = dp[0];
    }

    ArrayOf a = argIn[0];
    if (!a.is2D()) {
        Error(L"Nelson:constructors_functions:diagInputNot2D",
            L"First argument to 'diag' function must be 2D.");
    }
    ArrayOf diag;
    if (a.isSparse()) {
        diag = sparseDiagonal(a, diagonalOrder);
    } else if ((a.dimensionLength(1) == 1) || (a.dimensionLength(0) == 1)) {
        diag = ArrayOf::diagonalMatrix(a, diagonalOrder);
    } else {
        diag = a.diagonal(diagonalOrder);
    }
    // the diagonal of no elements holds no imaginary part: it is real
    retval << (a.isEmpty() ? decomplexify(diag) : diag);
    return retval;
}
//=============================================================================
