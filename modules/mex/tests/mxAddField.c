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
    int fieldnumber = -1;
    mxArray* ptr = NULL;
    if (nrhs != 1) {
        mexErrMsgTxt("Wrong number or type of input argument.");
    }
    ptr = mxDuplicateArray(prhs[0]);
    fieldnumber = mxAddField(ptr, "field_added");
    mxSetFieldByNumber(ptr, 0, fieldnumber, mxCreateDoubleScalar(44));
    plhs[0] = ptr;
}
//=============================================================================
