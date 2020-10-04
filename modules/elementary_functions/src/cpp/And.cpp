//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "And.hpp"
#include "MatrixCheck.hpp"
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
    indexType m = 0;
    indexType p = 0;
    for (indexType i = 0; i < N; i++) {

        C[i] = static_cast<Nelson::logical>(
            (static_cast<Nelson::logical>(A[m] != 0u) != 0u) && (B[p] != 0u));
        m += Astride;
        p += Bstride;
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
    if (A.isVector() && B.isVector()) {
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
    } else if (A.isScalar()) {
        Astride = 0;
        Bstride = 1;
        Cdim = B.getDimensions();
    } else if (B.isScalar()) {
        Astride = 1;
        Bstride = 0;
        Cdim = A.getDimensions();
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
                static_cast<const logical*>(A.getDataPointer()),
                A.getDimensions().getElementCount(),
                static_cast<const logical*>(B.getDataPointer()),
                B.getDimensions().getElementCount());
        } else if (A.isColumnVector() && B.isRowVector()) {
            boolean_vector_and(static_cast<logical*>(Cp),
                static_cast<const logical*>(B.getDataPointer()),
                B.getDimensions().getElementCount(),
                static_cast<const logical*>(A.getDataPointer()),
                A.getDimensions().getElementCount());
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
