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
    mxArray* trueScalar = mxCreateLogicalScalar(true);
    mxArray* falseScalar = mxCreateLogicalScalar(false);
    mxArray* numericScalar = mxCreateDoubleScalar(1);

    plhs[0] = mxCreateDoubleMatrix(1, 3, mxREAL);
    mxDouble* out = mxGetPr(plhs[0]);
    out[0] = mxIsLogicalScalarTrue(trueScalar);
    out[1] = mxIsLogicalScalarTrue(falseScalar);
    out[2] = mxIsLogicalScalarTrue(numericScalar);

    mxDestroyArray(trueScalar);
    mxDestroyArray(falseScalar);
    mxDestroyArray(numericScalar);

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
