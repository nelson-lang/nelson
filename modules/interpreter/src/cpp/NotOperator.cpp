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
#include "Not.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
ArrayOf
Evaluator::notOperator(ASTPtr t)
{
    pushID(t->context());
    ArrayOf retval = this->notOperator(expression(t->down));
    popID();
    return retval;
}
//=============================================================================
ArrayOf
Evaluator::notOperator(ArrayOf A)
{
    ArrayOf res;
    bool bSuccess = false;
    if ((overloadOnBasicTypes || needToOverloadOperator(A)) && !isOverloadAllowed()) {
        res = OverloadUnaryOperator(this, A, "not", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        res = Not(A, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(this, A, "not");
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
