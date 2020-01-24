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
#include "Not.hpp"
#include "Exception.hpp"
#include "Error.hpp"
#include "nlsConfig.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
boolean_not(indexType N, logical* C, const logical* A)
{
#if defined(_NLS_WITH_OPENMP)
#pragma omp parallel for
#endif
    for (ompIndexType i = 0; i < (ompIndexType)N; i++) {
        C[i] = static_cast<Nelson::logical>(static_cast<Nelson::logical>(A[i]) == 0u);
    }
}
//=============================================================================
ArrayOf
Not(ArrayOf& A, bool& needToOverload)
{
    ArrayOf C;
    needToOverload = false;
    if (A.getDataClass() == NLS_SCOMPLEX || A.getDataClass() == NLS_DCOMPLEX) {
        Error(_W("Input argument must be real."));
    }
    try {
        A.promoteType(NLS_LOGICAL);
    } catch (Exception&) {
        needToOverload = true;
        return ArrayOf();
    }
    logical* Cp = static_cast<logical*>(ArrayOf::allocateArrayOf(
        NLS_LOGICAL, A.getDimensions().getElementCount(), stringVector(), false));
    boolean_not(A.getLength(), Cp, static_cast<const logical*>(A.getDataPointer()));
    C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
    return C;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
