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
    if (nrhs != 0) {
        mexErrMsgIdAndTxt("Nelson:minrhs", "No input arguments required.");
    }
    if (nlhs > 1) {
        mexErrMsgIdAndTxt("Nelson:maxrhs", "Too many output arguments.");
    }
    plhs[0] = mxCreateString(mexFunctionName());
}
//=============================================================================
