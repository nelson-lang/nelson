//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "Not.hpp"
#include "Exception.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
boolean_not(indexType N, logical* C, const logical* A)
{
    for (indexType i = 0; i < N; i++) {
        C[i] = !A[i];
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
    logical* Cp = (logical*)ArrayOf::allocateArrayOf(
        NLS_LOGICAL, A.getDimensions().getElementCount(), stringVector(), false);
    boolean_not(A.getLength(), (logical*)Cp, (const logical*)A.getDataPointer());
    C = ArrayOf(NLS_LOGICAL, A.getDimensions(), Cp);
    return C;
}
//=============================================================================
}
//=============================================================================
