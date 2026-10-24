//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "And.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "omp_for_loop.hpp"
#include "MatrixCheck.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
#include <cmath>
#include <Eigen/Sparse>
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
static logical
logicalValueAtLinear(const ArrayOf& A, indexType idx)
{
    if (A.isSparse()) {
        const auto* spmat = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
            A.sparseDataPointer());
        indexType row = idx % A.rows();
        indexType col = idx / A.rows();
        return static_cast<logical>(spmat->coeff(row, col) != 0);
    }
    return static_cast<const logical*>(A.dataPointer())[idx] != 0;
}
//=============================================================================
static logical
logicalValueAtRowColumn(const ArrayOf& A, indexType row, indexType col)
{
    if (A.isSparse()) {
        const auto* spmat = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
            A.sparseDataPointer());
        return static_cast<logical>(spmat->coeff(row, col) != 0);
    }
    return static_cast<const logical*>(A.dataPointer())[row + col * A.rows()] != 0;
}
//=============================================================================
static void
boolean_vector_and(logical* C, const logical* A, indexType NA, const logical* B, indexType NB)
{
    indexType m = 0;
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            C[m] = A[i] & B[j];
            m++;
        }
    }
}
//=============================================================================
static void
boolean_and(indexType N, logical* C, const logical* A, int Astride, const logical* B, int Bstride)
{
    if (Astride == 1 && Bstride == 1) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[i] != 0u) && (B[i] != 0u));
        }

    } else if (Astride == 0 && Bstride == 1) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[0] != 0u) && (B[i] != 0u));
        }

    } else if (Astride == 1 && Bstride == 0) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[i] != 0u) && (B[0] != 0u));
        }
    }
}
//=============================================================================
ArrayOf
And(const ArrayOf& A, const ArrayOf& B, NelsonType commonType, bool& needToOverload)
{
    if (commonType > NLS_CHAR) {
        needToOverload = true;
        return {};
    }
    needToOverload = false;
    int Astride;
    int Bstride;
    indexType Clen = 0;
    Dimensions Cdim;
    ArrayOf _A(A);
    ArrayOf _B(B);
    if (!promoteToLogicalCheckedNaN(_A)) {
        _A.promoteType(NLS_LOGICAL);
    }
    if (!promoteToLogicalCheckedNaN(_B)) {
        _B.promoteType(NLS_LOGICAL);
    }
    Dimensions dimsA = _A.dimensions();
    Dimensions dimsB = _B.dimensions();
    if (!SameSizeCheck(dimsA, dimsB) && !_A.isScalar() && !_B.isScalar()) {
        BinaryBroadcast info = binaryOperatorBroadcast(_A, _B, "&");
        Clen = info.dimensions.elementCount();
        if ((_A.isSparse() || _B.isSparse()) && info.dimensions.length() <= 2) {
            auto* spmatDST = new Eigen::SparseMatrix<logical, 0, signedIndexType>(
                info.dimensions.rows(), info.dimensions.columns());
            for (indexType idx = 0; idx < Clen; ++idx) {
                indexType aIndex = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
                indexType bIndex = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
                if (logicalValueAtLinear(_A, aIndex) && logicalValueAtLinear(_B, bIndex)) {
                    spmatDST->coeffRef(idx % info.dimensions.rows(), idx / info.dimensions.rows())
                        = 1;
                }
            }
            ArrayOf C = makeSparseResult(NLS_LOGICAL, info.dimensions, spmatDST);
            C.resize(info.dimensions);
            return C;
        }
        void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), false);
        auto* ptrC = static_cast<logical*>(Cp);
        for (indexType idx = 0; idx < Clen; ++idx) {
            indexType aIndex = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
            indexType bIndex = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
            ptrC[idx] = logicalValueAtLinear(_A, aIndex) && logicalValueAtLinear(_B, bIndex);
        }
        return ArrayOf(NLS_LOGICAL, info.dimensions, Cp);
    }
    if (_A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = _B.dimensions();
    } else if (_B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = _A.dimensions();
    } else if (_A.isVector() && _B.isVector()) {
        if ((_A.isRowVector() && _B.isRowVector())
            || (_A.isColumnVector() && _B.isColumnVector())) {
            Astride = 1;
            Bstride = 1;
            Cdim = _A.dimensions();
        } else {
            Astride = 0;
            Bstride = 0;
            Dimensions dimsA = _A.dimensions();
            Dimensions dimsB = _B.dimensions();
            Cdim = Dimensions(std::min(dimsA.maximum(), dimsB.maximum()),
                std::max(dimsA.maximum(), dimsB.maximum()));
        }
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = _A.dimensions();
    }
    Clen = Cdim.elementCount();
    if (_A.isSparse() || _B.isSparse()) {
        auto* spmatDST
            = new Eigen::SparseMatrix<logical, 0, signedIndexType>(Cdim.rows(), Cdim.columns());
        if (Astride == 0 && Bstride == 0) {
            if (_A.isRowVector() && _B.isColumnVector()) {
                for (indexType col = 0; col < Cdim.columns(); ++col) {
                    logical a = logicalValueAtLinear(_A, col);
                    if (a) {
                        for (indexType row = 0; row < Cdim.rows(); ++row) {
                            if (logicalValueAtLinear(_B, row)) {
                                spmatDST->coeffRef(row, col) = 1;
                            }
                        }
                    }
                }
            } else if (_A.isColumnVector() && _B.isRowVector()) {
                for (indexType col = 0; col < Cdim.columns(); ++col) {
                    logical b = logicalValueAtLinear(_B, col);
                    if (b) {
                        for (indexType row = 0; row < Cdim.rows(); ++row) {
                            if (logicalValueAtLinear(_A, row)) {
                                spmatDST->coeffRef(row, col) = 1;
                            }
                        }
                    }
                }
            }
        } else if (Astride == 1 && Bstride == 1 && _A.isSparse() && _B.isSparse()) {
            // AND = intersection of the two true patterns, merged per column in
            // O(nnzA + nnzB) (no rows*cols scan).
            const auto* sa = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                _A.sparseDataPointer());
            const auto* sb = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                _B.sparseDataPointer());
            std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
            for (Eigen::Index col = 0; col < sa->outerSize(); ++col) {
                Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator ia(*sa, col);
                Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator ib(*sb, col);
                while (ia && ib) {
                    if (ia.row() < ib.row()) {
                        ++ia;
                    } else if (ib.row() < ia.row()) {
                        ++ib;
                    } else {
                        if (ia.value() && ib.value()) {
                            triplets.emplace_back(ia.row(), (signedIndexType)col, 1);
                        }
                        ++ia;
                        ++ib;
                    }
                }
            }
            spmatDST->setFromTriplets(triplets.begin(), triplets.end());
        } else if ((Astride == 0 && _B.isSparse()) || (Bstride == 0 && _A.isSparse())) {
            // scalar & sparse: nonzero only when the scalar is true, at the
            // sparse operand's true positions (O(nnz)).
            bool scalarIsA = (Astride == 0);
            const ArrayOf& scalarArg = scalarIsA ? _A : _B;
            const ArrayOf& matrixArg = scalarIsA ? _B : _A;
            if (logicalValueAtLinear(scalarArg, 0)) {
                const auto* sm
                    = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                        matrixArg.sparseDataPointer());
                std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
                triplets.reserve(sm->nonZeros());
                for (Eigen::Index col = 0; col < sm->outerSize(); ++col) {
                    for (Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator it(
                             *sm, col);
                        it; ++it) {
                        if (it.value()) {
                            triplets.emplace_back(it.row(), (signedIndexType)col, 1);
                        }
                    }
                }
                spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            }
        } else {
            for (indexType idx = 0; idx < Clen; ++idx) {
                indexType aIndex = Astride == 0 ? 0 : idx;
                indexType bIndex = Bstride == 0 ? 0 : idx;
                if (logicalValueAtLinear(_A, aIndex) && logicalValueAtLinear(_B, bIndex)) {
                    spmatDST->coeffRef(idx % Cdim.rows(), idx / Cdim.rows()) = 1;
                }
            }
        }
        ArrayOf C = makeSparseResult(NLS_LOGICAL, Cdim, spmatDST);
        if (_A.isSparse()) {
            C.setSparseNzmax(_A.nzmax());
        } else if (_B.isSparse()) {
            C.setSparseNzmax(_B.nzmax());
        }
        C.resize(Cdim);
        return C;
    }
    void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true);
    if (Astride == 0 && Bstride == 0) {
        if (_A.isRowVector() && _B.isColumnVector()) {
            boolean_vector_and(static_cast<logical*>(Cp),
                static_cast<const logical*>(_A.dataPointer()), _A.elementCount(),
                static_cast<const logical*>(_B.dataPointer()), _B.elementCount());
        } else if (_A.isColumnVector() && _B.isRowVector()) {
            boolean_vector_and(static_cast<logical*>(Cp),
                static_cast<const logical*>(_B.dataPointer()), _B.elementCount(),
                static_cast<const logical*>(_A.dataPointer()), _A.elementCount());
        }
    } else {
        boolean_and(Clen, static_cast<logical*>(Cp), static_cast<const logical*>(_A.dataPointer()),
            Astride, static_cast<const logical*>(_B.dataPointer()), Bstride);
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
