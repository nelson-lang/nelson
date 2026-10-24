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
    mwSize dims[2];
    dims[0] = 2;
    dims[1] = 3;
    mxArray* pa = mxCreateDoubleMatrix(2, 3, mxREAL);

    plhs[0] = mxCreateDoubleMatrix(1, 8, mxREAL);
    mxDouble* out = mxGetPr(plhs[0]);
    out[0] = mxGetNumberOfElements(NULL);
    out[1] = mxIsEmpty(NULL);
    out[2] = mxIsScalar(NULL);
    out[3] = mxGetM(NULL);
    out[4] = mxGetN(NULL);
    out[5] = mxSetDimensions(NULL, dims, 2);
    out[6] = mxSetDimensions(pa, NULL, 2);
    out[7] = mxCalcSingleSubscript(pa, 2, NULL);

    mxDestroyArray(pa);

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
