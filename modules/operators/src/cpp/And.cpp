//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "And.hpp"
#include "MatrixCheck.hpp"
#include "nlsBuildConfig.h"
#include <algorithm>
//=============================================================================
namespace Nelson {
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
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[i] != 0u) && (B[i] != 0u));
        }

    } else if (Astride == 0 && Bstride == 1) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[0] != 0u) && (B[i] != 0u));
        }

    } else if (Astride == 1 && Bstride == 0) {
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
        for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
            C[i] = static_cast<Nelson::logical>((A[i] != 0u) && (B[0] != 0u));
        }
    }
}
//=============================================================================
ArrayOf
And(ArrayOf A, ArrayOf B)
{
    int Astride;
    int Bstride;
    indexType Clen = 0;
    Dimensions Cdim;
    BoolVectorCheck(A, B, "&");
    if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = B.getDimensions();
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = A.getDimensions();
    } else if (A.isVector() && B.isVector()) {
        if ((A.isRowVector() && B.isRowVector()) || (A.isColumnVector() && B.isColumnVector())) {
            Astride = 1;
            Bstride = 1;
            Cdim = A.getDimensions();
        } else {
            Astride = 0;
            Bstride = 0;
            Dimensions dimsA = A.getDimensions();
            Dimensions dimsB = B.getDimensions();
            Cdim = Dimensions(
                std::min(dimsA.getMax(), dimsB.getMax()), std::max(dimsA.getMax(), dimsB.getMax()));
        }
    } else {
        Astride = 1;
        Bstride = 1;
        Cdim = A.getDimensions();
    }
    Clen = Cdim.getElementCount();
    void* Cp = ArrayOf::allocateArrayOf(NLS_LOGICAL, Clen, stringVector(), true);
    if (Astride == 0 && Bstride == 0) {
        if (A.isRowVector() && B.isColumnVector()) {
            boolean_vector_and(static_cast<logical*>(Cp),
                static_cast<const logical*>(A.getDataPointer()), A.getElementCount(),
                static_cast<const logical*>(B.getDataPointer()), B.getElementCount());
        } else if (A.isColumnVector() && B.isRowVector()) {
            boolean_vector_and(static_cast<logical*>(Cp),
                static_cast<const logical*>(B.getDataPointer()), B.getElementCount(),
                static_cast<const logical*>(A.getDataPointer()), A.getElementCount());
        }
    } else {
        boolean_and(Clen, static_cast<logical*>(Cp),
            static_cast<const logical*>(A.getDataPointer()), Astride,
            static_cast<const logical*>(B.getDataPointer()), Bstride);
    }
    return ArrayOf(NLS_LOGICAL, Cdim, Cp);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
