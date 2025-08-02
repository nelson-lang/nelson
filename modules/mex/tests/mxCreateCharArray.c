//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string.h>
#include <mex.h>
//=============================================================================
#define TOTAL_ELEMENTS 4
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    int bytes_to_copy;
    mxChar* dataptr = NULL;
    mxChar data[] = { 'a', 'b', 'c', 'd' };
    mwSize dims[2] = { 2, 2 };
    if (nrhs != 0) {
        mexErrMsgIdAndTxt("Nelson:minrhs", "No input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("Nelson:maxrhs", "Too many output arguments.");
    }
    plhs[0] = mxCreateCharArray(2, (const mwSize*)dims);
    dataptr = (mxChar*)mxGetData(plhs[0]);
    bytes_to_copy = TOTAL_ELEMENTS * mxGetElementSize(plhs[0]);
    memcpy(dataptr, data, bytes_to_copy);
}
//=============================================================================
