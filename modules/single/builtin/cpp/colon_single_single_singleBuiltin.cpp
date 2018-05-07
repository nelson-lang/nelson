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
#include "colon_single_single_singleBuiltin.hpp"
#include "ColonSingle.hpp"
#include "Error.hpp"
#include "MatrixCheck.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::SingleGateway::colon_single_single_singleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 3)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    ArrayOf C = argIn[2];
    if (!A.isSingleType() || !B.isSingleType() || !C.isSingleType())
    {
        Error(eval, ERROR_WRONG_ARGUMENTS_TYPE_SINGLE_EXPECTED);
    }
    ArrayOfVector retval;
    ArrayOf res = colon_single(A, C, B);
    retval.push_back(res);
    return retval;
}
//=============================================================================
