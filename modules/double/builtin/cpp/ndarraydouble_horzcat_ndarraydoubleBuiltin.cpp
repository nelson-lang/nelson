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
#include "ndarraydouble_horzcat_ndarraydoubleBuiltin.hpp"
#include "Error.hpp"
#include "HorzCatDouble.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::DoubleGateway::ndarraydouble_horzcat_ndarraydoubleBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
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
    ArrayOf A = argIn[0];
    ArrayOf B = argIn[1];
    if (!A.isNdArrayDoubleType())
    {
        Error(eval, ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED);
    }
    if (!B.isNdArrayDoubleType())
    {
        Error(eval, ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED);
    }
    Dimensions dimsA = A.getDimensions();
    Dimensions dimsB = B.getDimensions();
    if (dimsA.getRows() != dimsB.getRows())
    {
        Error(eval, ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    if (dimsA.getLength() != dimsB.getLength())
    {
        Error(eval, ERROR_DIMENSIONS_NOT_CONSISTENT);
    }
    for (indexType k = 0; k < dimsA.getLength(); k++)
    {
        if (k != 1)
        {
            if (dimsA.getDimensionLength(k) != dimsB.getDimensionLength(k))
            {
                Error(eval, ERROR_DIMENSIONS_NOT_CONSISTENT);
            }
        }
    }
    retval.push_back(HorzCatNdArrayDouble(A, B));
    return retval;
}
//=============================================================================
