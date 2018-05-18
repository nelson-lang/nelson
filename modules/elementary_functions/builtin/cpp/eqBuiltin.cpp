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
#include "eqBuiltin.hpp"
#include "Error.hpp"
#include "OverloadBinaryOperator.hpp"
#include "OverloadRequired.hpp"
#include "Equals.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::ElementaryFunctionsGateway::eqBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 2)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    bool bSuccess = false;
    ArrayOf res = OverloadBinaryOperator(eval, argIn[0], argIn[1], "eq", bSuccess);
    if (bSuccess)
    {
        retval.push_back(res);
        return retval;
    }
    else
    {
        if ((argIn[0].getDataClass() == NLS_HANDLE) || (argIn[0].getDataClass() == NLS_STRUCT_ARRAY) || (argIn[0].getDataClass() == NLS_CELL_ARRAY) ||
                (argIn[1].getDataClass() == NLS_HANDLE) || (argIn[1].getDataClass() == NLS_STRUCT_ARRAY) || (argIn[1].getDataClass() == NLS_CELL_ARRAY) ||
                (argIn[0].isSparse() || argIn[1].isSparse()) )
        {
            OverloadRequired(eval, argIn, Nelson::BINARY);
        }
		ArrayOf A = argIn[0];
		ArrayOf B = argIn[1];
        retval.push_back(Equals(A, B, true, bSuccess));
    }
    return retval;
}
//=============================================================================
