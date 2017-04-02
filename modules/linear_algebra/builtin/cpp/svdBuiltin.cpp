//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "svdBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "OverloadRequired.hpp"
#include "SVD.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::LinearAlgebraGateway::svdBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 3)
    {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, bSuccess);
    if (!bSuccess)
    {
        if ((argIn[0].getDataClass() == NLS_STRUCT_ARRAY) ||
                (argIn[0].getDataClass() == NLS_CELL_ARRAY) ||
                argIn[0].isSparse() ||
                argIn[0].isLogical() ||
                argIn[0].isString() ||
                argIn[0].isIntegerType())
        {
            OverloadRequired(eval, argIn, Nelson::FUNCTION);
        }
        switch (nLhs)
        {
            case 0:
            case 1:
            {
                ArrayOf s;
                SVD(argIn[0], s);
                retval.push_back(s);
            }
            break;
            case 2:
            {
                ArrayOf U;
                ArrayOf S;
                SVD(argIn[0], U, S);
                retval.push_back(U);
                retval.push_back(S);
            }
            break;
            case 3:
            {
                ArrayOf U;
                ArrayOf S;
                ArrayOf V;
                SVD(argIn[0], U, S, V);
                retval.push_back(U);
                retval.push_back(S);
                retval.push_back(V);
            }
            break;
            default:
                Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
                break;
        }
    }
    return retval;
}
//=============================================================================
