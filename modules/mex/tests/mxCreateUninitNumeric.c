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
    mxArray* doubleMatrix = mxCreateUninitNumericMatrix(2, 1, mxDOUBLE_CLASS, mxCOMPLEX);
    mxComplexDouble* doubleData = mxGetComplexDoubles(doubleMatrix);
    doubleData[0].real = 1.5;
    doubleData[0].imag = 2.5;
    doubleData[1].real = 3.5;
    doubleData[1].imag = 4.5;

    size_t dims[2];
    dims[0] = 2;
    dims[1] = 3;
    mxArray* singleArray = mxCreateUninitNumericArray(2, dims, mxSINGLE_CLASS, mxCOMPLEX);
    mxComplexSingle* singleData = mxGetComplexSingles(singleArray);
    mxSingle realSum = 0;
    mxSingle imagSum = 0;
    mwSize elementCount = mxGetNumberOfElements(singleArray);
    mwSize k;
    for (k = 0; k < elementCount; ++k) {
        singleData[k].real = (mxSingle)(k + 1);
        singleData[k].imag = (mxSingle)(10 + k);
        realSum = (mxSingle)(realSum + singleData[k].real);
        imagSum = (mxSingle)(imagSum + singleData[k].imag);
    }

    plhs[0] = mxCreateDoubleMatrix(1, 7, mxREAL);
    mxDouble* out = mxGetDoubles(plhs[0]);
    out[0] = doubleData[0].real;
    out[1] = doubleData[0].imag;
    out[2] = doubleData[1].real;
    out[3] = doubleData[1].imag;
    out[4] = elementCount;
    out[5] = realSum;
    out[6] = imagSum;

    mxDestroyArray(doubleMatrix);
    mxDestroyArray(singleArray);
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
