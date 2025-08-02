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
    if (nrhs != 1) {
        mexErrMsgTxt("Wrong number or type of input argument");
    }
    plhs[0] = mxDuplicateArray(prhs[0]);
}
//=============================================================================
