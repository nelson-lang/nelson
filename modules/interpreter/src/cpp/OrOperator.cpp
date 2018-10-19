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
#include "Or.hpp"
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::orOperator(ArrayOf A, ArrayOf B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        retval = OverloadBinaryOperator(this, A, B, "or");
    } else {
        retval = Or(A, B);
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::orOperator(ASTPtr t)
{
    pushID(t->context());
    const ArrayOf A = expression(t->down);
    const ArrayOf B = expression(t->down->right);
    ArrayOf retval = orOperator(A, B);
    popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
