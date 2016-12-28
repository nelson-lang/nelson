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
#include <string>
#include "datevecBuiltin.hpp"
#include "Error.hpp"
#include "DateVector.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::TimeGateway::datevecBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() != 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    double dt = param1.getContentsAsDoubleScalar();
    double Y = 0, M = 0, D = 0, H = 0, MM = 0, S = 0;
    DateVector(dt, Y, M, D, H, MM, S);
    ArrayOfVector retval;
    if (nLhs < 2)
    {
        double *dval = (double *)ArrayOf::allocateArrayOf(NLS_DOUBLE, 6);
        dval[0] = Y;
        dval[1] = M;
        dval[2] = D;
        dval[3] = H;
        dval[4] = MM;
        dval[5] = S;
        Dimensions dim(1, 6);
        retval.push_back(ArrayOf(NLS_DOUBLE, dim, dval));
    }
    else
    {
        if (nLhs > 6)
        {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        else
        {
            if (nLhs > 1)
            {
                retval.push_back(ArrayOf::doubleConstructor(Y));
                retval.push_back(ArrayOf::doubleConstructor(M));
            }
            if (nLhs > 2)
            {
                retval.push_back(ArrayOf::doubleConstructor(D));
            }
            if (nLhs > 3)
            {
                retval.push_back(ArrayOf::doubleConstructor(H));
            }
            if (nLhs > 4)
            {
                retval.push_back(ArrayOf::doubleConstructor(MM));
            }
            if (nLhs > 5)
            {
                retval.push_back(ArrayOf::doubleConstructor(S));
            }
        }
    }
    return retval;
}
//=============================================================================
