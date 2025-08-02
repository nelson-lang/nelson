//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mex.h>
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    (void)plhs;
    (void)prhs;

    if (nrhs != 0) {
        mexErrMsgIdAndTxt("Nelson:mexevalstring:nInput", "No input arguments required.");
    }
    if (nlhs != 0) {
        mexErrMsgIdAndTxt("Nelson:mexevalstring:nOutput", "Too many output arguments.");
    }

    mexEvalString("A = 'Hello World'");
    mexEvalString("B = 100");
}
//=============================================================================
