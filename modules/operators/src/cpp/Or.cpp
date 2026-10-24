//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Or.hpp"
#include "BinaryOperatorsHelpers.hpp"
#include "MatrixCheck.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "SparseType.hpp"
#include "SparseArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
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
static void
boolean_vector_or(logical* C, const logical* A, indexType NA, const logical* B, indexType NB)
{
    indexType m = 0;
    for (indexType i = 0; i < NA; i++) {
        for (indexType j = 0; j < NB; j++) {
            C[m] = A[i] | B[j];
            m++;
        }
    }
}
//=============================================================================
static void
boolean_or(indexType N, logical* C, const logical* A, int Astride, const logical* B, int Bstride)
{
    if (Astride == 1 && Bstride == 1) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = A[i] | B[i];
        }
    } else if (Astride == 0 && Bstride == 1) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = A[0] | B[i];
        }
    } else if (Astride == 1 && Bstride == 0) {
        OMP_PARALLEL_FOR_LOOP(N)
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = A[i] | B[0];
        }
    }
}
//=============================================================================
ArrayOf
Or(const ArrayOf& A, const ArrayOf& B, NelsonType commonType, bool& needToOverload)
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
    bool bothSparse = _A.isSparse() && _B.isSparse();
    if (!promoteToLogicalCheckedNaN(_A)) {
        _A.promoteType(NLS_LOGICAL);
    }
    if (!promoteToLogicalCheckedNaN(_B)) {
        _B.promoteType(NLS_LOGICAL);
    }
    Dimensions dimsA = _A.dimensions();
    Dimensions dimsB = _B.dimensions();
    if (!SameSizeCheck(dimsA, dimsB) && !_A.isScalar() && !_B.isScalar()) {
        BinaryBroadcast info = binaryOperatorBroadcast(_A, _B, "|");
        Clen = info.dimensions.elementCount();
        if (bothSparse && info.dimensions.length() <= 2) {
            auto* spmatDST = new Eigen::SparseMatrix<logical, 0, signedIndexType>(
                info.dimensions.rows(), info.dimensions.columns());
            for (indexType idx = 0; idx < Clen; ++idx) {
                indexType aIndex = binaryOperatorBroadcastIndex(info, idx, info.aStrides);
                indexType bIndex = binaryOperatorBroadcastIndex(info, idx, info.bStrides);
                if (logicalValueAtLinear(_A, aIndex) || logicalValueAtLinear(_B, bIndex)) {
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
            ptrC[idx] = logicalValueAtLinear(_A, aIndex) || logicalValueAtLinear(_B, bIndex);
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
        if (bothSparse) {
            auto* spmatDST
                = new Eigen::SparseMatrix<logical, 0, signedIndexType>(Cdim.rows(), Cdim.columns());
            if (Astride == 0 && Bstride == 0) {
                if (_A.isRowVector() && _B.isColumnVector()) {
                    for (indexType col = 0; col < Cdim.columns(); ++col) {
                        logical a = logicalValueAtLinear(_A, col);
                        for (indexType row = 0; row < Cdim.rows(); ++row) {
                            if (a || logicalValueAtLinear(_B, row)) {
                                spmatDST->coeffRef(row, col) = 1;
                            }
                        }
                    }
                } else if (_A.isColumnVector() && _B.isRowVector()) {
                    for (indexType col = 0; col < Cdim.columns(); ++col) {
                        logical b = logicalValueAtLinear(_B, col);
                        for (indexType row = 0; row < Cdim.rows(); ++row) {
                            if (logicalValueAtLinear(_A, row) || b) {
                                spmatDST->coeffRef(row, col) = 1;
                            }
                        }
                    }
                }
            } else if (Astride == 1 && Bstride == 1) {
                // OR = union of the two true patterns, merged per column in
                // O(nnzA + nnzB) (no rows*cols scan).
                const auto* sa
                    = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                        _A.sparseDataPointer());
                const auto* sb
                    = static_cast<const Eigen::SparseMatrix<logical, 0, signedIndexType>*>(
                        _B.sparseDataPointer());
                std::vector<Eigen::Triplet<logical, signedIndexType>> triplets;
                for (Eigen::Index col = 0; col < sa->outerSize(); ++col) {
                    Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator ia(*sa, col);
                    Eigen::SparseMatrix<logical, 0, signedIndexType>::InnerIterator ib(*sb, col);
                    while (ia || ib) {
                        if (ia && (!ib || ia.row() < ib.row())) {
                            if (ia.value()) {
                                triplets.emplace_back(ia.row(), (signedIndexType)col, 1);
                            }
                            ++ia;
                        } else if (ib && (!ia || ib.row() < ia.row())) {
                            if (ib.value()) {
                                triplets.emplace_back(ib.row(), (signedIndexType)col, 1);
                            }
                            ++ib;
                        } else {
                            if (ia.value() || ib.value()) {
                                triplets.emplace_back(ia.row(), (signedIndexType)col, 1);
                            }
                            ++ia;
                            ++ib;
                        }
                    }
                }
                spmatDST->setFromTriplets(triplets.begin(), triplets.end());
            } else {
                for (indexType idx = 0; idx < Clen; ++idx) {
                    indexType aIndex = Astride == 0 ? 0 : idx;
                    indexType bIndex = Bstride == 0 ? 0 : idx;
                    if (logicalValueAtLinear(_A, aIndex) || logicalValueAtLinear(_B, bIndex)) {
                        spmatDST->coeffRef(idx % Cdim.rows(), idx / Cdim.rows()) = 1;
                    }
                }
            }
            ArrayOf C = makeSparseResult(NLS_LOGICAL, Cdim, spmatDST);
            C.resize(Cdim);
            return C;
        }
        void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), false);
        auto* ptrC = static_cast<logical*>(Cp);
        if (Astride == 0 && Bstride == 0) {
            if (_A.isRowVector() && _B.isColumnVector()) {
                for (indexType col = 0; col < Cdim.columns(); ++col) {
                    logical a = logicalValueAtLinear(_A, col);
                    for (indexType row = 0; row < Cdim.rows(); ++row) {
                        ptrC[row + col * Cdim.rows()] = a || logicalValueAtLinear(_B, row);
                    }
                }
            } else if (_A.isColumnVector() && _B.isRowVector()) {
                for (indexType col = 0; col < Cdim.columns(); ++col) {
                    logical b = logicalValueAtLinear(_B, col);
                    for (indexType row = 0; row < Cdim.rows(); ++row) {
                        ptrC[row + col * Cdim.rows()] = logicalValueAtLinear(_A, row) || b;
                    }
                }
            }
        } else {
            for (indexType idx = 0; idx < Clen; ++idx) {
                indexType aIndex = Astride == 0 ? 0 : idx;
                indexType bIndex = Bstride == 0 ? 0 : idx;
                ptrC[idx] = logicalValueAtLinear(_A, aIndex) || logicalValueAtLinear(_B, bIndex);
            }
        }
        return ArrayOf(NLS_LOGICAL, Cdim, Cp);
    }
    void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), false);
    if (Astride == 0 && Bstride == 0) {
        if (_A.isRowVector() && _B.isColumnVector()) {
            boolean_vector_or(static_cast<logical*>(Cp),
                static_cast<const logical*>(_A.dataPointer()), _A.elementCount(),
                static_cast<const logical*>(_B.dataPointer()), _B.elementCount());
        } else if (_A.isColumnVector() && _B.isRowVector()) {
            boolean_vector_or(static_cast<logical*>(Cp),
                static_cast<const logical*>(_B.dataPointer()), _B.elementCount(),
                static_cast<const logical*>(_A.dataPointer()), _A.elementCount());
        }
    } else {
        boolean_or(Clen, static_cast<logical*>(Cp), static_cast<const logical*>(_A.dataPointer()),
            Astride, static_cast<const logical*>(_B.dataPointer()), Bstride);
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
