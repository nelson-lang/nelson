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
#include <string.h>
#include <mex.h>
//=============================================================================
#define TOTAL_ELEMENTS 4
//=============================================================================
void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    int bytes_to_copy;
    mxChar *dataptr = NULL;
    mxChar data[] = {'a', 'b', 'c', 'd'};
    mwSize dims[2] = {2, 2};
    if (nrhs !=0 ) {
        mexErrMsgIdAndTxt( "NELSON:minrhs",
                "No input arguments required.");
    } 
    if(nlhs > 1){
        mexErrMsgIdAndTxt( "NELSON:maxrhs",
                "Too many output arguments.");
    }
    plhs[0]= mxCreateCharArray(2, (const int *)dims);  
    dataptr = (mxChar *)mxGetData(plhs[0]);   
    bytes_to_copy = TOTAL_ELEMENTS * mxGetElementSize(plhs[0]);    
    memcpy(dataptr, data, bytes_to_copy);
}
//=============================================================================
