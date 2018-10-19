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
#include "uminusBuiltin.hpp"
#include "Error.hpp"
#include "UnaryMinus.hpp"
#include "OverloadUnaryOperator.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::uminusBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf arg1 = argIn[0];
    bool bSuccess = false;
    ArrayOf a = argIn[0];
    ArrayOf res;
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "uminus", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = UnaryMinus(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "uminus");
            if (bSuccess) {
                retval.push_back(res);
            }
        } else {
            retval.push_back(res);
        }
    } else {
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
