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
#include "Power.hpp"
#include "DotPower.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
namespace Nelson {
ArrayOf
Power(ArrayOf A, ArrayOf B)
{
    if (A.isEmpty() || B.isEmpty()) {
        return ArrayOf::emptyConstructor();
    }
    if (A.isScalar() && B.isScalar()) {
        return DotPower(A, B);
    }
    // Check for A & B numeric
    CheckNumeric(A, B, "^");
    // Test for 2D on both A & B
    if (!A.is2D() || !B.is2D()) {
        Error(_W("Cannot apply exponential operation to N-Dimensional arrays."));
    }
    // Both arguments must be square
    if ((A.getDimensionLength(0) != A.getDimensionLength(1))
        || (B.getDimensionLength(0) != B.getDimensionLength(1))) {
        Error(
            _W("Power (^) operator can only be applied to scalar and square arguments."));
    }
    Error(_W("Power (^) currently not implemented in Nelson."));
}
}
//=============================================================================
