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
#include "ctransposeBuiltin.hpp"
#include "Error.hpp"
#include "OverloadUnaryOperator.hpp"
#include "ComplexTranspose.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::ctransposeBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    ArrayOf res;
    ArrayOf a = argIn[0];
    if (eval->mustOverloadBasicTypes()) {
        res = OverloadUnaryOperator(eval, a, "ctranspose", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload = false;
        res = ComplexTranspose(a, needToOverload);
        if (needToOverload) {
            res = OverloadUnaryOperator(eval, a, "ctranspose", bSuccess);
        }
    }
    retval.push_back(res);
    return retval;
}
//=============================================================================
