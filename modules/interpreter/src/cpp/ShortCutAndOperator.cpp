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
#include "Evaluator.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::shortCutAndOperator(ArrayOf A, ArrayOf B)
{
    ArrayOf retval;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        retval = OverloadBinaryOperator(this, A, B, "shortcutand");
    } else {
        if (A.isScalar() && B.isScalar()) {
            bool a = A.getContentAsLogicalScalar();
            if (!a) {
                retval = A;
            } else {
                bool b = B.getContentAsLogicalScalar();
                return ArrayOf::logicalConstructor(a && b);
            }
        } else {
            Error(_W("Operand to && operator must be convertible to "
                     "logical scalar values."));
        }
    }
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::shortCutAndOperator(ASTPtr t)
{
    pushID(t->context());
    ArrayOf A = expression(t->down);
    ArrayOf B = expression(t->down->right);
    ArrayOf retval = shortCutAndOperator(A, B);
    popID();
    return retval;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
