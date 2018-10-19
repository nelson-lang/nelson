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
#include "strncmpBuiltin.hpp"
#include "Error.hpp"
#include "StringCompare.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static ArrayOfVector
strncmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool bCaseSensitive)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C = argIn[2];
    indexType len = C.getContentAsScalarIndex(false);
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        if (bCaseSensitive) {
            retval = OverloadFunction(eval, nLhs, argIn, "strncmp", bSuccess);
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "strncmpi", bSuccess);
        }
    }
    if (!bSuccess) {
        retval.push_back(StringCompare(A, B, bCaseSensitive, len));
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(eval, nLhs, argIn, true);
}
//=============================================================================
ArrayOfVector
Nelson::StringGateway::strncmpiBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    return ::strncmpBuiltin(eval, nLhs, argIn, false);
}
//=============================================================================
