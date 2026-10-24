//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Not.hpp"
#include "NelsonException.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <Eigen/Sparse>
#include <cmath>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
template <class T>
static void
convertToLogicalCheckedNaN(const T* src, logical* dst, indexType count)
{
    for (indexType k = 0; k < count; ++k) {
        if (std::isnan(src[k])) {
            Error(L"Nelson:operators:nanToLogical",
                L"Conversion to logical with NaN is not possible.");
        }
        dst[k] = static_cast<logical>(src[k] != T(0));
    }
}
//=============================================================================
// Converts A in place to NLS_LOGICAL, raising an error when a NaN value is
// present. Returns false when A must still be converted with promoteType.
static bool
promoteToLogicalCheckedNaN(ArrayOf& A)
{
    NelsonType dataClass = A.dataClass();
    if (A.isSparse()) {
        if (dataClass == NLS_DOUBLE) {
            const auto* spmat = static_cast<const Eigen::SparseMatrix<double, 0, signedIndexType>*>(
                A.sparseDataPointer());
            for (Eigen::Index k = 0; k < spmat->outerSize(); ++k) {
                for (Eigen::SparseMatrix<double, 0, signedIndexType>::InnerIterator it(*spmat, k);
                    it; ++it) {
                    if (std::isnan(it.value())) {
                        Error(L"Nelson:operators:nanToLogical",
                            L"Conversion to logical with NaN is not possible.");
                    }
                }
            }
        }
        return false;
    }
    if (dataClass != NLS_DOUBLE && dataClass != NLS_SINGLE) {
        return false;
    }
    indexType count = A.elementCount();
    logical* pLogical = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, count, stringVector(), false));
    ArrayOf res(NLS_LOGICAL, A.dimensions(), pLogical);
    if (dataClass == NLS_DOUBLE) {
        convertToLogicalCheckedNaN<double>(
            static_cast<const double*>(A.dataPointer()), pLogical, count);
    } else {
        convertToLogicalCheckedNaN<single>(
            static_cast<const single*>(A.dataPointer()), pLogical, count);
    }
    A = res;
    return true;
}
//=============================================================================
static void
boolean_not(indexType N, logical* C, const logical* A)
{
    OMP_PARALLEL_FOR_LOOP(N)
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = static_cast<Nelson::logical>(static_cast<Nelson::logical>(A[i]) == 0u);
    }
}
//=============================================================================
template <class T>
static ArrayOf
SparseNotTemplate(const ArrayOf& A)
{
    indexType rows = A.rows();
    indexType cols = A.columns();
    const auto* spmatSRC
        = static_cast<const Eigen::SparseMatrix<T, 0, signedIndexType>*>(A.sparseDataPointer());
    std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
    triplets.reserve(A.elementCount() - A.nnz());
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            if (spmatSRC->coeff(row, col) == T(0)) {
                triplets.emplace_back(row, col, static_cast<logical>(1));
            }
        }
    }
    auto* spmatDST = new Eigen::SparseMatrix<logical, 0, signedIndexType>(rows, cols);
    if (!triplets.empty()) {
        spmatDST->setFromTriplets(triplets.begin(), triplets.end());
    }
    return makeSparseResult(NLS_LOGICAL, A.dimensions(), spmatDST);
}
//=============================================================================
static ArrayOf
SparseNot(const ArrayOf& A, bool& needToOverload)
{
    switch (A.dataClass()) {
    case NLS_LOGICAL:
        return SparseNotTemplate<logical>(A);
    case NLS_DOUBLE:
        return SparseNotTemplate<double>(A);
    default:
        needToOverload = true;
        return {};
    }
}
//=============================================================================
ArrayOf
Not(const ArrayOf& A, bool& needToOverload)
{
    ArrayOf C;
    needToOverload = false;
    if (A.dataClass() == NLS_SCOMPLEX || A.dataClass() == NLS_DCOMPLEX) {
        Error(
            L"Nelson:operators:invalidBitInput:inputArgumentReal", L"Input argument must be real.");
    }
    ArrayOf AA = A;
    // raises an error on NaN values (dense and sparse double/single)
    bool promoted = promoteToLogicalCheckedNaN(AA);
    if (A.isSparse()) {
        return SparseNot(A, needToOverload);
    }
    if (!promoted) {
        try {
            AA.promoteType(NLS_LOGICAL);
        } catch (NelsonException&) {
            needToOverload = true;
            return {};
        }
    }
    logical* Cp = static_cast<logical*>(
        ArrayOf::allocateArrayOf(NLS_LOGICAL, A.elementCount(), stringVector(), false));
    boolean_not(A.elementCount(), Cp, static_cast<const logical*>(AA.dataPointer()));
    C = ArrayOf(NLS_LOGICAL, A.dimensions(), Cp);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
