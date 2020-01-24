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
#include "XorLogical.hpp"
#include "MatrixCheck.hpp"
#include "Types.hpp"
#include "Exception.hpp"
#include "nlsConfig.h"
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
    size_t m = 0;
    size_t p = 0;
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)n; i++) {
        c[i] = NLSXOR(a[m], b[p]);
        m += (size_t)stride1;
        p += (size_t)stride2;
    }
}
//=============================================================================
ArrayOf
XorLogical(ArrayOf A, ArrayOf B)
{
    ArrayOf C;
    if ((A.getDataClass() == NLS_LOGICAL) && (B.getDataClass() == NLS_LOGICAL)) {
        Dimensions dimsA = A.getDimensions();
        Dimensions dimsB = B.getDimensions();
        if (!(SameSizeCheck(dimsA, dimsB) || A.isScalar() || B.isScalar())) {
            Error(_W("Size mismatch on arguments."));
        }
        if (A.isScalar()) {
            size_t Blen(B.getLength());
            auto* Cp = new_with_exception<logical>(Blen, false);
            boolXor(Blen, Cp, (logical*)A.getDataPointer(), 0, (logical*)B.getDataPointer(), 1);
            C = ArrayOf(NLS_LOGICAL, B.getDimensions(), Cp);
        } else if (B.isScalar()) {
            size_t Alen(A.getLength());
            auto* Cp = new_with_exception<logical>(Alen, false);
            boolXor(Alen, Cp, (logical*)A.getDataPointer(), 1, (logical*)B.getDataPointer(), 0);
            C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
        } else {
            size_t Alen(A.getLength());
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
