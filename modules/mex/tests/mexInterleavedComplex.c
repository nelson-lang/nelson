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
    mxArray* v = mxCreateDoubleMatrix(1, 1, mxCOMPLEX);
#if defined(MX_HAS_INTERLEAVED_COMPLEX)
    mxComplexDouble* data = mxGetComplexDoubles(v);
    data->real = 1.53;
    data->imag = 1.63;
#else
    double* re_data = mxGetPr(v);
    double* im_data = mxGetPi(v);
    *re_data = 1.73;
    *im_data = 4.76;
#endif
    plhs[0] = v;
    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
