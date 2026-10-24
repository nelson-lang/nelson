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
    if (nrhs != 1) {
        mexErrMsgTxt("One input argument expected.");
    }
    mxArray* st = mxDuplicateArray(prhs[0]);
    mxArray* missingValue = mxCreateDoubleScalar(11);
    mxArray* outOfRangeValue = mxCreateDoubleScalar(12);

    mxArray* validField = mxGetField(st, 0, "b");
    mxArray* missingField = mxGetField(st, 0, "missing");
    mxArray* nullNameField = mxGetField(st, 0, NULL);
    mxSetField(st, 0, "missing", missingValue);
    mxSetField(st, 0, NULL, missingValue);
    mxSetFieldByNumber(st, 99, 0, outOfRangeValue);
    mxSetFieldByNumber(st, 0, -1, outOfRangeValue);

    plhs[0] = mxCreateDoubleMatrix(1, 4, mxREAL);
    mxDouble* out = mxGetPr(plhs[0]);
    out[0] = validField != NULL ? mxGetScalar(validField) : -1;
    out[1] = missingField == NULL;
    out[2] = nullNameField == NULL;
    out[3] = mxGetFieldNumber(st, "missing") == -1;

    mxDestroyArray(validField);
    mxDestroyArray(missingValue);
    mxDestroyArray(outOfRangeValue);
    mxDestroyArray(st);

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
