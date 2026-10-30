//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// A compiled Mandelbrot escape-time kernel callable from Nelson as a MEX.
// Call: counts = mandelbrot_mex_kernel(centerX, centerY, span, N, maxIter)
#include "mex.h"
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    if (nrhs != 5) {
        mexErrMsgTxt("Five inputs expected: centerX, centerY, span, N, maxIter.");
    }
    double centerX = mxGetScalar(prhs[0]);
    double centerY = mxGetScalar(prhs[1]);
    double span = mxGetScalar(prhs[2]);
    int side = (int)mxGetScalar(prhs[3]);
    int maxIterations = (int)mxGetScalar(prhs[4]);
    plhs[0] = mxCreateDoubleMatrix(side, side, mxREAL);
    double* out = mxGetPr(plhs[0]);
    double start = -span / 2.0;
    double step = span / (double)(side - 1);
    // Column-major fill: each column is a fixed real part.
    for (int col = 0; col < side; col++) {
        double cr = centerX + start + col * step;
        for (int row = 0; row < side; row++) {
            double ci = centerY + start + row * step;
            double zr = 0.0;
            double zi = 0.0;
            int k = 0;
            while (k < maxIterations && zr * zr + zi * zi <= 4.0) {
                double zrNext = zr * zr - zi * zi + cr;
                zi = 2.0 * zr * zi + ci;
                zr = zrNext;
                k++;
            }
            out[col * side + row] = (double)k;
        }
    }
}
//=============================================================================
