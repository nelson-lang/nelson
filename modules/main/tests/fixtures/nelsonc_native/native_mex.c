//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// SPDX-License-Identifier: LGPL-3.0-or-later
//=============================================================================
#include "mex.h"

#ifdef _WIN32
__declspec(dllimport) double
nc_native_bridge(double value);
#else
extern double
nc_native_bridge(double value);
#endif

void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nlhs != 1 || nrhs != 1 || !mxIsDouble(prhs[0]) || mxGetNumberOfElements(prhs[0]) != 1) {
        mexErrMsgTxt("One double scalar input and one output are required.");
    }
    plhs[0] = mxCreateDoubleScalar(nc_native_bridge(mxGetScalar(prhs[0])));
}
