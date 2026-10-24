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
#if defined(MX_HAS_INTERLEAVED_COMPLEX)
    mxArray* pa = mxCreateDoubleMatrix(1, 2, mxREAL);
    mxDouble* realValues = mxGetDoubles(pa);
    realValues[0] = 3;
    realValues[1] = 4;

    int madeComplex = mxMakeArrayComplex(pa);
    mxComplexDouble* complexValues = mxGetComplexDoubles(pa);
    int complexStateOk = madeComplex == 1 && mxIsComplex(pa) && complexValues[0].real == 3
        && complexValues[0].imag == 0 && complexValues[1].real == 4 && complexValues[1].imag == 0;

    complexValues[0].imag = 7;
    complexValues[1].imag = 8;
    int madeReal = mxMakeArrayReal(pa);
    realValues = mxGetDoubles(pa);

    plhs[0] = mxCreateDoubleMatrix(1, 5, mxREAL);
    mxDouble* out = mxGetDoubles(plhs[0]);
    out[0] = complexStateOk;
    out[1] = madeReal;
    out[2] = mxIsComplex(pa);
    out[3] = realValues[0];
    out[4] = realValues[1];
    mxDestroyArray(pa);
#else
    plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
    mxGetPr(plhs[0])[0] = -1;
#endif

    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
}
//=============================================================================
