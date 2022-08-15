//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <mex.h>
//=============================================================================
static void
CallAtExit(void)
{
    mexPrintf("Call at Exit");
}
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 0) {
        mexErrMsgTxt("Wrong number or type of input argument");
    }
    mexAtExit(CallAtExit);
    mexLock();
    mexUnlock();
}
//=============================================================================
