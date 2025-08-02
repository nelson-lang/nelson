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
    if (nlhs > 1) {
        mexErrMsgTxt("Wrong number of output argument.");
    }
    if (nrhs != 1) {
        mexErrMsgTxt("Wrong number of input argument.");
    }
    if (!mxIsStruct(prhs[0])) {
        mexErrMsgTxt("Struct expected");
    }
    mxSetClassName(prhs[0], "complexObj");
    plhs[0] = mxDuplicateArray(prhs[0]);
}
//=============================================================================
