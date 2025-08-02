//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XorLogical.hpp"
#include "MatrixCheck.hpp"
#include "Types.hpp"
#include "NewWithException.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "Error.hpp"
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
ArrayOf
XorLogical(const ArrayOf& A, const ArrayOf& B)
{
    ArrayOf C;
    if ((A.getDataClass() == NLS_LOGICAL) && (B.getDataClass() == NLS_LOGICAL)) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
            Error(_W("Size mismatch on arguments."));
        }
        if (A.isScalar()) {
            size_t Blen(B.getElementCount());
            auto* Cp = new_with_exception<logical>(Blen, false);
            boolXor(Blen, Cp, (logical*)A.getDataPointer(), 0, (logical*)B.getDataPointer(), 1);
            C = ArrayOf(NLS_LOGICAL, B.getDimensions(), Cp);
        } else if (B.isScalar()) {
            size_t Alen(A.getElementCount());
            auto* Cp = new_with_exception<logical>(Alen, false);
            boolXor(Alen, Cp, (logical*)A.getDataPointer(), 1, (logical*)B.getDataPointer(), 0);
            C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
        } else {
            size_t Alen(A.getElementCount());
            auto* Cp = new_with_exception<logical>(Alen, false);
            boolXor(Alen, Cp, (logical*)A.getDataPointer(), 1, (logical*)B.getDataPointer(), 1);
            C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
        }
    } else {
        Error(_W("Invalid type."));
    }
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
