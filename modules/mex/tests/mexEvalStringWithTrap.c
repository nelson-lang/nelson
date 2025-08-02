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
    if (nlhs != 1) {
        mexErrMsgIdAndTxt("Nelson:mexevalstring:nOutput", "One output arguments required.");
    }
    mxArray* MException;
    if (nrhs == 1) {
        MException = mexEvalStringWithTrap("B = 100");
    } else {
        MException = mexEvalStringWithTrap("NOT_EXIST");
    }
    if (MException == NULL) {
        plhs[0] = mxCreateString("It works !!!");
    } else {
        plhs[0] = MException;
    }
}
//=============================================================================
