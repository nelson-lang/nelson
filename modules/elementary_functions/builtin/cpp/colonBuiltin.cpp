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
#include "colonBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
#include "OverloadTernaryOperator.hpp"
#include "Colon.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::colonBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    bool needToOverload;
    bool bSuccess = false;
    ArrayOf res;
    if (argIn.size() == 2) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        if (eval->mustOverloadBasicTypes()) {
            res = OverloadBinaryOperator(eval, A, B, "colon", false, bSuccess, "");
        }
        if (!bSuccess) {
            res = Colon(A, B, needToOverload);
            if (needToOverload) {
                res = OverloadBinaryOperator(eval, A, B, "colon");
            }
        }
    } else if (argIn.size() == 3) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        ArrayOf C = argIn[2];
        if (eval->mustOverloadBasicTypes()) {
            res = OverloadTernaryOperator(eval, A, B, C, "colon", false, bSuccess, "");
        }
        if (!bSuccess) {
            res = Colon(A, B, C, needToOverload);
            if (needToOverload) {
                res = OverloadTernaryOperator(eval, A, B, C, "colon");
            }
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    retval.push_back(res);
    return retval;
}
//=============================================================================
