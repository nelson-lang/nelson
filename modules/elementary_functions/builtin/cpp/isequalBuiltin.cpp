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
#include "isequalBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ElementaryFunctionsGateway::isequalBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() < 2) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    for (size_t k = 1; k < argIn.size(); k++) {
        ArrayOfVector v1v2;
        v1v2.push_back(argIn[k - 1]);
        v1v2.push_back(argIn[k]);
        // Call overload if it exists
        bool bSuccess = false;
        retval = OverloadFunction(eval, nLhs, v1v2, "isequal", bSuccess);
        if (!bSuccess) {
            OverloadRequired(eval, v1v2, Overload::OverloadClass::FUNCTION);
        }
        bool res = false;
        if (retval.size() > 0) {
            res = retval[0].getContentAsLogicalScalar() == 0 ? false : true;
        } else {
            Error(eval, _W("overload of isequal must return a logical."));
        }
        if (!res) {
            return retval;
        }
    }
    return retval;
}
//=============================================================================
