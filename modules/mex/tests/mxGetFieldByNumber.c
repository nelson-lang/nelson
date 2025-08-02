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
    mxArray* pOut = NULL;
    if (nrhs != 3) {
        mexErrMsgTxt("Wrong number or type of input argument");
    }
    if (!mxIsStruct(prhs[0])) {
        mexErrMsgTxt("Struct expected");
    }
    mwIndex index = (mwIndex)mxGetScalar(prhs[1]);
    int fieldnumber = (int)mxGetScalar(prhs[2]);
    pOut = mxGetFieldByNumber(prhs[0], index, fieldnumber);
    if (pOut == NULL)
        pOut = mxCreateLogicalScalar(false);
    plhs[0] = pOut;
}
//=============================================================================
