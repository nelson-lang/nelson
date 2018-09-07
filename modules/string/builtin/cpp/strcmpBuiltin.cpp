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
#include "strcmpBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "StringCompare.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strcmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 2) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        if (bCaseSensitive) {
            retval = OverloadFunction(eval, nLhs, argIn, "strcmp", bSuccess);
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "strcmpi", bSuccess);
        }
    }
    if (!bSuccess) {
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        retval.push_back(StringCompare(A, B, bCaseSensitive));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strcmpiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strcmpBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
