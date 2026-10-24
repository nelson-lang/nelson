//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <matrix.h>
//=============================================================================
#ifndef MX_API_VER
#error MX_API_VER is not defined
#endif
#ifndef MX_LAST_32BIT_VER
#error MX_LAST_32BIT_VER is not defined
#endif
#ifndef MX_TARGET_API_VER
#error MX_TARGET_API_VER is not defined
#endif
#ifndef MX_HAS_64BIT_ARRAY_DIMS
#error MX_HAS_64BIT_ARRAY_DIMS is not defined
#endif
//=============================================================================
static void
localFunction(int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[])
{
}
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    mxFunctionPtr ptr = localFunction;
    plhs[0] = mxCreateDoubleMatrix(1, 4, mxREAL);
    mxDouble* out = mxGetDoubles(plhs[0]);
    out[0] = MX_API_VER == MX_CURRENT_API_VER;
    out[1] = MX_TARGET_API_VER == MX_CURRENT_API_VER;
    out[2] = MX_HAS_64BIT_ARRAY_DIMS != 0;
    out[3] = ptr != 0;

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
