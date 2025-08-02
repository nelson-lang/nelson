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
    if (nrhs != 2) {
        mexErrMsgTxt("Wrong number of input argument.");
    }
    char* workspace = mxArrayToString(prhs[0]);
    char* name = mxArrayToString(prhs[1]);
    mxArray* pOut = mexGetVariable(workspace, name);
    plhs[0] = pOut != NULL ? pOut : mxCreateString("ERROR");
}
//=============================================================================
