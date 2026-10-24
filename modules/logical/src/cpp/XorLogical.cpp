//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "XorLogical.hpp"
#include "MatrixCheck.hpp"
#include "Types.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static inline logical
NLSXOR(logical a, logical b)
{
    return (!a && b) || (a && !b);
}
//=============================================================================
static void
boolXor(
    size_t n, logical* c, const logical* a, const int stride1, const logical* b, const int stride2)
{
    if (stride1 == 1 && stride2 == 1) {
        OMP_PARALLEL_FOR_LOOP(n)
        for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
            c[i] = NLSXOR(a[i], b[i]);
        }
    } else {
        if (stride1 == 0 && stride2 == 1) {
            OMP_PARALLEL_FOR_LOOP(n)
            for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
                c[i] = NLSXOR(a[0], b[i]);
            }
        } else if (stride1 == 1 && stride2 == 0) {
            OMP_PARALLEL_FOR_LOOP(n)
            for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
                c[i] = NLSXOR(a[i], b[0]);
            }
        }
    }
}
//=============================================================================
// implicit expansion: each dimension of A and B is equal or 1
static ArrayOf
broadcastXor(const ArrayOf& A, const ArrayOf& B)
{
    Dimensions dimsA = A.dimensions();
    Dimensions dimsB = B.dimensions();
    indexType nDims = std::max(dimsA.length(), dimsB.length());
    Dimensions dimsC(nDims);
    indexType strideA[maxDims];
    indexType strideB[maxDims];
    indexType offsetA = 1;
    indexType offsetB = 1;
    for (indexType k = 0; k < nDims; ++k) {
        indexType dimA = dimsA.dimensionLength(k);
        indexType dimB = dimsB.dimensionLength(k);
        if (dimA != dimB && dimA != 1 && dimB != 1) {
            Error(L"Nelson:dimagree:sizeMismatchArguments", L"Size mismatch on arguments.");
        }
        dimsC[k] = (dimA == 1) ? dimB : dimA;
        strideA[k] = (dimA == 1) ? 0 : offsetA;
        strideB[k] = (dimB == 1) ? 0 : offsetB;
        offsetA *= dimA;
        offsetB *= dimB;
    }
    indexType Clen = dimsC.elementCount();
    auto* Cp = new_with_exception<logical>(Clen, false);
    const auto* a = static_cast<const logical*>(A.dataPointer());
    const auto* b = static_cast<const logical*>(B.dataPointer());
    Dimensions position(nDims);
    for (indexType i = 0; i < Clen; ++i) {
        indexType indexA = 0;
        indexType indexB = 0;
        for (indexType k = 0; k < nDims; ++k) {
            indexA += position[k] * strideA[k];
            indexB += position[k] * strideB[k];
        }
        Cp[i] = NLSXOR(a[indexA], b[indexB]);
        position.incrementModulo(dimsC, 0);
    }
    dimsC.simplify();
    return ArrayOf(NLS_LOGICAL, dimsC, Cp);
}
//=============================================================================
ArrayOf
XorLogical(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf C;
    if ((A.dataClass() == NLS_LOGICAL) && (B.dataClass() == NLS_LOGICAL)) {
        Dimensions dimsA = A.dimensions();
        Dimensions dimsB = B.dimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
            return broadcastXor(A, B);
        }
        if (A.isScalar()) {
            size_t Blen(B.elementCount());
            auto* Cp = new_with_exception<logical>(Blen, false);
            boolXor(Blen, Cp, (logical*)A.dataPointer(), 0, (logical*)B.dataPointer(), 1);
            C = ArrayOf(NLS_LOGICAL, B.dimensions(), Cp);
        } else if (B.isScalar()) {
            size_t Alen(A.elementCount());
            auto* Cp = new_with_exception<logical>(Alen, false);
            boolXor(Alen, Cp, (logical*)A.dataPointer(), 1, (logical*)B.dataPointer(), 0);
            C = ArrayOf(NLS_LOGICAL, A.dimensions(), Cp);
        } else {
            size_t Alen(A.elementCount());
            auto* Cp = new_with_exception<logical>(Alen, false);
            boolXor(Alen, Cp, (logical*)A.dataPointer(), 1, (logical*)B.dataPointer(), 1);
            C = ArrayOf(NLS_LOGICAL, A.dimensions(), Cp);
        }
    } else {
        Error(L"Nelson:xor:invalidType", L"Invalid type.");
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
