//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <mex.h>
//=============================================================================
void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    if(nrhs != 1)
    {
       mexErrMsgTxt("Wrong number or type of input argument");
    }
    int expr = (int)mxGetScalar(prhs[0]);
    switch (expr){
        case 0: {
            plhs[0] = mxCreateDoubleMatrix(2, 3, mxREAL);
        } break;
        case 1: {
            plhs[0] = mxCreateDoubleMatrix(2, 3, mxCOMPLEX);
        } break;
        case 2: {
            plhs[0] = mxCreateDoubleMatrix(1, 2, mxREAL);
            mxDouble *U = mxGetPr(plhs[0]);
            U[0] = 1;
            U[1] = 2;
        } break;
        case 3: {
            plhs[0] = mxCreateDoubleMatrix(1, 2, mxCOMPLEX);
            mxDouble *Ur = mxGetPr(plhs[0]);
            mxDouble *Ui = mxGetPi(plhs[0]);
            Ur[0] = 1;
            Ur[1] = 2;
            Ui[0] = 3;
            Ui[1] = 4;
        } break;
        default:{
            mexErrMsgTxt("Wrong input value.");
        }break;
    }

}
//=============================================================================
