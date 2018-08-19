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
#include "logical_allBuiltin.hpp"
#include "AllLogical.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::LogicalGateway::logical_allBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (!((argIn.size() == 1) || (argIn.size() == 2))) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    indexType d = 0;
    ArrayOf arg1 = argIn[0];
    if (argIn.size() > 1) {
        ArrayOf arg2 = argIn[1];
        d = arg2.getContentAsScalarIndex(false);
    }
    if (!arg1.isLogical()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    if (arg1.isSparse()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED);
    }
    retval.push_back(AllLogical(arg1, d));
    return retval;
}
//=============================================================================
