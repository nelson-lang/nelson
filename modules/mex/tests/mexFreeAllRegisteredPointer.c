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
    void* p1 = mxMalloc(8);
    void* p2 = mxCalloc(4, 2);
    void* p3 = mxMalloc(16);
    bool registeredBefore
        = mxIsRegisteredPointer(p1) && mxIsRegisteredPointer(p2) && mxIsRegisteredPointer(p3);

    mexFreeAllRegisteredPointer();

    bool unregisteredAfter
        = !mxIsRegisteredPointer(p1) && !mxIsRegisteredPointer(p2) && !mxIsRegisteredPointer(p3);

    plhs[0] = mxCreateDoubleMatrix(1, 2, mxREAL);
    mxDouble* out = mxGetPr(plhs[0]);
    out[0] = registeredBefore;
    out[1] = unregisteredAfter;

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
