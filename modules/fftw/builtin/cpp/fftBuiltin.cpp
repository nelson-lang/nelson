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
#include "fftBuiltin.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "Fft.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector Nelson::FftwGateway::fftBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() < 1)
    {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    retval = OverloadFunction(eval, nLhs, argIn, bSuccess);
    if (!bSuccess)
    {
        if (nLhs > 1)
        {
            Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        ArrayOf res;
        switch (argIn.size())
        {
            case 1:
            {
                res = Fft(argIn[0]);
            }
            break;
            case 2:
            {
                ArrayOf X = argIn[0];
                ArrayOf N = argIn[1];
                if (N.isNumeric() && N.isEmpty())
                {
                    res = Fft(X);
                }
                else
                {
                    indexType n = N.getContentAsScalarIndex(false);
                    res = Fft(X, n);
                }
            }
            break;
            case 3:
            {
                ArrayOf X = argIn[0];
                ArrayOf N = argIn[1];
                ArrayOf DIM = argIn[2];
                indexType dim = DIM.getContentAsScalarIndex(false);
                indexType n;
                if(N.isNumeric() && N.isEmpty())
                {
                    // fft(X, [], dim)
					if (X.isScalar())
					{
						n = 1;
					}
					else
					{
						n = X.getDimensionLength((int)dim);
					}
                }
                else
                {
                    // fft(X, n, dim)
                    n = N.getContentAsScalarIndex(false);
                }
                res = Fft(X, n, dim);
            }
            break;
            default:
            {
                Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
            }
            break;
        }
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
