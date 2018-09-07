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
#include "strrepBuiltin.hpp"
#include "Error.hpp"
#include "StringReplace.hpp"
#include "OverloadFunction.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strrepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "strrep", bSuccess);
    }
    if (!bSuccess) {
        bool needToOverload;
        ArrayOf res = StringReplace(argIn[0], argIn[1], argIn[2], true, needToOverload);
        if (needToOverload) {
            retval = OverloadFunction(eval, nLhs, argIn, "strrep", bSuccess);
            if (!bSuccess) {
                Error(_W("Invalid input argument(s): cell or string expected."));
            }
        } else {
            retval.push_back(res);
        }
    }
    return retval;
}
//=============================================================================
