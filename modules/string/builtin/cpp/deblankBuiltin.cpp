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
#include "deblankBuiltin.hpp"
#include "Error.hpp"
#include "StringDeblank.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::deblankBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "deblank", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = StringDeblank(A, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "deblank", bSuccess);
            if (!bSuccess) {
                Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED);
            }
        } else {
            retval.push_back(res);
        }
    }
    return retval;
}
//=============================================================================
