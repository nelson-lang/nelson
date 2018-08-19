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
#include "sparselogical_realBuiltin.hpp"
#include "Error.hpp"
#include "SparseRealPart.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::SparseGateway::sparselogical_realBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (!argIn[0].isSparse()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_LOGICAL_EXPECTED);
    }
    if (!argIn[0].isLogical()) {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_LOGICAL_EXPECTED);
    }
    retval.push_back(SparseRealPart(argIn[0]));
    return retval;
}
//=============================================================================
