//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Or.hpp"
#include "MatrixCheck.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <algorithm>
//=============================================================================
namespace Nelson {
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
    if (commonType > NLS_CHAR || (A.isSparse() || B.isSparse())) {
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
    PromoteToLogicalVectorCheck(_A, _B, "|");
    if (_A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = _B.getDimensions();
    } else if (_B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = _A.getDimensions();
    } else if (_A.isVector() && _B.isVector()) {
        if ((_A.isRowVector() && _B.isRowVector())
            || (_A.isColumnVector() && _B.isColumnVector())) {
            Astride = 1;
            Bstride = 1;
            Cdim = _A.getDimensions();
        } else {
            Astride = 0;
            Bstride = 0;
            Dimensions dimsA = _A.getDimensions();
            Dimensions dimsB = _B.getDimensions();
            Cdim = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
        }
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = _A.getDimensions();
    }
    Clen = Cdim.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), false);
    if (Astride == 0 && Bstride == 0) {
        if (_A.isRowVector() && _B.isColumnVector()) {
            boolean_vector_or(static_cast<logical*>(Cp),
                static_cast<const logical*>(_A.getDataPointer()), _A.getElementCount(),
                static_cast<const logical*>(_B.getDataPointer()), _B.getElementCount());
        } else if (_A.isColumnVector() && _B.isRowVector()) {
            boolean_vector_or(static_cast<logical*>(Cp),
                static_cast<const logical*>(_B.getDataPointer()), _B.getElementCount(),
                static_cast<const logical*>(_A.getDataPointer()), _A.getElementCount());
        }
    } else {
        boolean_or(Clen, static_cast<logical*>(Cp),
            static_cast<const logical*>(_A.getDataPointer()), Astride,
            static_cast<const logical*>(_B.getDataPointer()), Bstride);
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
