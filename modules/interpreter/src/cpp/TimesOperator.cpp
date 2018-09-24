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
#include "ElementWiseMultiplication.hpp"
#include "OverloadBinaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::timesOperator(ASTPtr t)
{
    pushID(t->context());
    ArrayOf retval = this->timesOperator(expression(t->down), expression(t->down->right));
    popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::timesOperator(ArrayOf A, ArrayOf B)
{
    ArrayOf res;
    bool bSuccess = false;
    if ((overloadOnBasicTypes || needToOverloadOperator(A) || needToOverloadOperator(B))
        && !isOverloadAllowed()) {
        res = OverloadBinaryOperator(this, A, B, "times", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = elementWiseMultiplication(A, B, needToOverload);
        if (needToOverload) {
            res = OverloadBinaryOperator(this, A, B, "times");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
