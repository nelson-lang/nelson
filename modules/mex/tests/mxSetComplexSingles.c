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
    mxArray* pa = mxCreateNumericMatrix(2, 1, mxSINGLE_CLASS, mxCOMPLEX);
    mxComplexSingle* pd = mxGetComplexSingles(pa);
    mxComplexSingle* dt = mxMalloc(2 * sizeof(mxComplexSingle));
    dt[0].real = 3.f;
    dt[0].imag = 9.f;
    dt[1].real = 4.f;
    dt[1].imag = 8.f;
    mxFree(pd);
    mxSetComplexSingles(pa, dt);
    plhs[0] = pa;
    int i;
    for (i = 1; i < nlhs; i++) {
        plhs[i] = mxCreateDoubleMatrix(0, 0, mxREAL);
    }
#endif
}
//=============================================================================
