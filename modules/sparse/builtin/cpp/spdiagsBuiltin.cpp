//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "spdiagsBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include <Eigen/Sparse>
#include <algorithm>
#include <cmath>
#include <set>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool
isSupportedSparseDataClass(NelsonType dataClass)
{
    return dataClass == NLS_DOUBLE || dataClass == NLS_DCOMPLEX || dataClass == NLS_SINGLE
        || dataClass == NLS_SCOMPLEX || dataClass == NLS_LOGICAL;
}
//=============================================================================
static std::vector<signedIndexType>
diagonalOffsets(const ArrayOf& D)
{
    ArrayOf dArray(D);
    dArray.promoteType(NLS_DOUBLE);
    const double* dptr = static_cast<const double*>(dArray.dataPointer());
    indexType diagCount = dArray.elementCount();
    std::vector<signedIndexType> offsets(diagCount);
    for (indexType k = 0; k < diagCount; ++k) {
        signedIndexType offset = static_cast<signedIndexType>(std::llround(dptr[k]));
        if (std::fabs(dptr[k] - static_cast<double>(offset)) > 0) {
            Error(L"Nelson:spdiags:offsetsMustBeIntegers", L"Diagonal offsets must be integers.");
        }
        offsets[k] = offset;
    }
    return offsets;
}
//=============================================================================
static bool
isReplacedDiagonal(
    signedIndexType row, signedIndexType col, const std::vector<signedIndexType>& offsets)
{
    signedIndexType offset = col - row;
    for (signedIndexType currentOffset : offsets) {
        if (currentOffset == offset) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static NelsonType
spdiagsReplacementDataClass(const ArrayOf& B, const ArrayOf& A)
{
    NelsonType bClass = B.dataClass();
    NelsonType aClass = A.dataClass();
    if (!isSupportedSparseDataClass(bClass)) {
        Error(L"Nelson:spdiags:unsupportedType", L"Unsupported type in %s.", L"first input");
    }
    if (!isSupportedSparseDataClass(aClass)) {
        Error(L"Nelson:spdiags:unsupportedType", L"Unsupported type in %s.", L"third input");
    }
    if (bClass == NLS_DCOMPLEX || aClass == NLS_DCOMPLEX) {
        return NLS_DCOMPLEX;
    }
    if ((bClass == NLS_SCOMPLEX || aClass == NLS_SCOMPLEX)
        && (bClass == NLS_DOUBLE || aClass == NLS_DOUBLE)) {
        return NLS_DCOMPLEX;
    }
    if (bClass == NLS_DOUBLE || aClass == NLS_DOUBLE) {
        return NLS_DOUBLE;
    }
    if (bClass == NLS_SCOMPLEX || aClass == NLS_SCOMPLEX) {
        return NLS_SCOMPLEX;
    }
    if (bClass == NLS_SINGLE || aClass == NLS_SINGLE) {
        return NLS_SINGLE;
    }
    return NLS_LOGICAL;
}
//=============================================================================
template <class T>
static ArrayOfVector
spdiagsExtract(const ArrayOf& A, int nLhs)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    const auto* spmat = static_cast<const SparseMatrix*>(A.sparseDataPointer());
    std::set<signedIndexType> offsets;
    for (signedIndexType col = 0; col < spmat->outerSize(); ++col) {
        for (typename SparseMatrix::InnerIterator it(*spmat, col); it; ++it) {
            offsets.insert(it.col() - it.row());
        }
    }

    indexType rowCount = std::min(A.rows(), A.columns());
    indexType diagCount = static_cast<indexType>(offsets.size());
    auto* ptrB = static_cast<T*>(
        ArrayOf::allocateArrayOf(A.dataClass(), rowCount * diagCount, stringVector(), true));
    double* ptrD = nullptr;
    if (nLhs > 1) {
        ptrD = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, diagCount));
    }

    std::vector<signedIndexType> d(offsets.begin(), offsets.end());
    for (indexType k = 0; k < diagCount; ++k) {
        if (ptrD != nullptr) {
            ptrD[k] = static_cast<double>(d[k]);
        }
        for (signedIndexType col = 0; col < spmat->outerSize(); ++col) {
            signedIndexType row = col - d[k];
            if (row >= 0 && row < spmat->rows() && col < static_cast<signedIndexType>(rowCount)) {
                T value = spmat->coeff(row, col);
                if (value != T(0)) {
                    ptrB[col + k * rowCount] = value;
                }
            }
        }
    }

    ArrayOfVector retval;
    retval << ArrayOf(A.dataClass(), Dimensions(rowCount, diagCount), ptrB);
    if (nLhs > 1) {
        retval << ArrayOf(NLS_DOUBLE, Dimensions(diagCount, 1), ptrD);
    }
    return retval;
}
//=============================================================================
template <class T>
static ArrayOf
spdiagsBuild(const ArrayOf& B, const ArrayOf& D, indexType m, indexType n)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<signedIndexType> offsets = diagonalOffsets(D);
    indexType diagCount = static_cast<indexType>(offsets.size());
    if (diagCount != B.columns()) {
        Error(L"Nelson:spdiags:columnDiagonalMismatch",
            L"The number of columns of the first input must match the number of diagonals.");
    }

    const T* bptr = static_cast<const T*>(B.dataPointer());
    indexType rowsB = B.rows();
    std::vector<Triplet> triplets;
    triplets.reserve(static_cast<size_t>(rowsB * diagCount));
    for (indexType k = 0; k < diagCount; ++k) {
        signedIndexType offset = offsets[k];
        for (indexType col = 0; col < n; ++col) {
            signedIndexType row = static_cast<signedIndexType>(col) - offset;
            if (row >= 0 && row < static_cast<signedIndexType>(m) && col < rowsB) {
                T value = bptr[col + k * rowsB];
                if (value != T(0)) {
                    triplets.emplace_back(row, static_cast<signedIndexType>(col), value);
                }
            }
        }
    }
    auto* spmat = new SparseMatrix(m, n);
    spmat->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseResult(B.dataClass(), Dimensions(m, n), spmat);
}
//=============================================================================
template <class T>
static ArrayOf
spdiagsReplace(const ArrayOf& B, const ArrayOf& D, const ArrayOf& A)
{
    using SparseMatrix = Eigen::SparseMatrix<T, 0, signedIndexType>;
    using Triplet = Eigen::Triplet<T, signedIndexType>;
    std::vector<signedIndexType> offsets = diagonalOffsets(D);
    indexType diagCount = static_cast<indexType>(offsets.size());
    if (diagCount != B.columns()) {
        Error(L"Nelson:spdiags:columnDiagonalMismatch",
            L"The number of columns of the first input must match the number of diagonals.");
    }

    indexType rows = A.rows();
    indexType cols = A.columns();
    indexType rowsB = B.rows();
    const T* bptr = static_cast<const T*>(B.dataPointer());
    const auto* src = static_cast<const SparseMatrix*>(A.sparseDataPointer());

    std::vector<Triplet> triplets;
    size_t replacementCount = static_cast<size_t>(std::min(rowsB, cols) * diagCount);
    triplets.reserve((src != nullptr ? src->nonZeros() : 0) + replacementCount);
    if (src != nullptr) {
        for (signedIndexType col = 0; col < src->outerSize(); ++col) {
            for (typename SparseMatrix::InnerIterator it(*src, col); it; ++it) {
                if (!isReplacedDiagonal(it.row(), it.col(), offsets) && it.value() != T(0)) {
                    triplets.emplace_back(it.row(), it.col(), it.value());
                }
            }
        }
    }
    for (indexType k = 0; k < diagCount; ++k) {
        signedIndexType offset = offsets[k];
        for (indexType col = 0; col < cols; ++col) {
            signedIndexType row = static_cast<signedIndexType>(col) - offset;
            if (row >= 0 && row < static_cast<signedIndexType>(rows) && col < rowsB) {
                T value = bptr[col + k * rowsB];
                if (value != T(0)) {
                    triplets.emplace_back(row, static_cast<signedIndexType>(col), value);
                }
            }
        }
    }

    auto* spmat = new SparseMatrix(rows, cols);
    spmat->setFromTriplets(triplets.begin(), triplets.end());
    return makeSparseResult(B.dataClass(), A.dimensions(), spmat);
}
//=============================================================================
static ArrayOfVector
spdiagsBuildDispatch(const ArrayOfVector& argIn)
{
    ArrayOf B(argIn[0]);
    if (B.isSparse()) {
        Error(L"Nelson:spdiags:fullMatrixExpected", L"Full matrix expected as first input.");
    }
    indexType m = argIn[2].asScalarIndex();
    indexType n = argIn[3].asScalarIndex();
    ArrayOfVector retval;
    switch (B.dataClass()) {
    case NLS_DOUBLE:
        retval << spdiagsBuild<double>(B, argIn[1], m, n);
        break;
    case NLS_DCOMPLEX:
        retval << spdiagsBuild<doublecomplex>(B, argIn[1], m, n);
        break;
    case NLS_SINGLE:
        retval << spdiagsBuild<single>(B, argIn[1], m, n);
        break;
    case NLS_SCOMPLEX:
        retval << spdiagsBuild<singlecomplex>(B, argIn[1], m, n);
        break;
    case NLS_LOGICAL:
        retval << spdiagsBuild<logical>(B, argIn[1], m, n);
        break;
    default:
        Error(L"Nelson:spdiags:unsupportedType", L"Unsupported type in %s.", L"first input");
    }
    return retval;
}
//=============================================================================
static ArrayOfVector
spdiagsReplaceDispatch(const ArrayOfVector& argIn)
{
    ArrayOf B(argIn[0]);
    if (B.isSparse()) {
        Error(L"Nelson:spdiags:fullMatrixExpected", L"Full matrix expected as first input.");
    }
    ArrayOf A(argIn[2]);
    NelsonType outputClass = spdiagsReplacementDataClass(B, A);
    B.promoteType(outputClass);
    A.promoteType(outputClass);
    if (!A.isSparse()) {
        A.makeSparse();
    }

    ArrayOfVector retval;
    switch (outputClass) {
    case NLS_DOUBLE:
        retval << spdiagsReplace<double>(B, argIn[1], A);
        break;
    case NLS_DCOMPLEX:
        retval << spdiagsReplace<doublecomplex>(B, argIn[1], A);
        break;
    case NLS_SINGLE:
        retval << spdiagsReplace<single>(B, argIn[1], A);
        break;
    case NLS_SCOMPLEX:
        retval << spdiagsReplace<singlecomplex>(B, argIn[1], A);
        break;
    case NLS_LOGICAL:
        retval << spdiagsReplace<logical>(B, argIn[1], A);
        break;
    default:
        Error(L"Nelson:spdiags:unsupportedType", L"Unsupported type in %s.", L"sparse matrix");
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::spdiagsBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 4);
    nargoutcheck(nLhs, 0, 2);

    if (argIn.size() == 1) {
        ArrayOf A(argIn[0]);
        if (!A.isSparse()) {
            A.makeSparse();
        }
        switch (A.dataClass()) {
        case NLS_DOUBLE:
            return spdiagsExtract<double>(A, nLhs);
        case NLS_DCOMPLEX:
            return spdiagsExtract<doublecomplex>(A, nLhs);
        case NLS_SINGLE:
            return spdiagsExtract<single>(A, nLhs);
        case NLS_SCOMPLEX:
            return spdiagsExtract<singlecomplex>(A, nLhs);
        case NLS_LOGICAL:
            return spdiagsExtract<logical>(A, nLhs);
        default:
            Error(L"Nelson:spdiags:unsupportedType", L"Unsupported type in %s.", L"sparse matrix");
        }
    }
    if (argIn.size() == 4) {
        return spdiagsBuildDispatch(argIn);
    }
    if (argIn.size() == 3) {
        return spdiagsReplaceDispatch(argIn);
    }
    Error(L"Nelson:wrongNumberOfInputs", L"Wrong number of input arguments.");
    return {};
}
//=============================================================================
