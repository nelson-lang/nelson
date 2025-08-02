//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <math.h>
#include <mex.h>
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    mwSize m = 0;
    mwSize n = 0;
    mwSize nzmax = 0;
    mwIndex* irs = NULL;
    mwIndex* jcs = NULL;
    mwIndex j = 0;
    mwIndex k = 0;
    mxLogical* pl = NULL;
    mxLogical* sl = NULL;
    double* pr = NULL;
    double* pi = NULL;
    double* si = NULL;
    double* sr = NULL;
    double percent = 0.2;
    bool isLogical = false;
    bool isComplex = false;
    mxArray* res = NULL;
    if (nlhs > 1) {
        mexErrMsgTxt("Too many output arguments.");
    }
    if (nrhs != 1) {
        mexErrMsgTxt("One input argument required.");
    }
    bool is_supported = mxIsDouble(prhs[0]) || mxIsLogical(prhs[0]);
    if (!is_supported) {
        mexErrMsgTxt("Input argument must be of type double or logical.");
    }
    if (mxGetNumberOfDimensions(prhs[0]) != 2) {
        mexErrMsgTxt("Input argument must be two dimensional.");
    }
    isComplex = mxIsComplex(prhs[0]);
    m = mxGetM(prhs[0]);
    n = mxGetN(prhs[0]);
    nzmax = (mwSize)ceil((double)m * (double)n * percent);
    isLogical = mxIsLogical(prhs[0]);
    if (isLogical) {
        res = mxCreateSparseLogicalMatrix(m, n, nzmax);
        pl = (mxLogical*)mxGetPr(prhs[0]);
    } else {
        res = mxCreateSparse(m, n, nzmax, isComplex);
        pr = mxGetPr(prhs[0]);
        pi = mxGetPi(prhs[0]);
    }
    if (res) {
        irs = mxGetIr(res);
        jcs = mxGetJc(res);
        if (isLogical) {
            sl = (mxLogical*)mxGetPr(res);
        } else {
            sr = mxGetPr(res);
            si = mxGetPi(res);
        }
        for (j = 0; j < n; j++) {
            mwSize i = 0;
            jcs[j] = k;
            if (isLogical) {
                for (i = 0; i < m; i++) {
                    if (pl[i] == true) {
                        sl[k] = pl[i];
                        irs[k] = i;
                        k++;
                    }
                }
                pl += m;
            } else {
                for (i = 0; i < m; i++) {
                    if ((pr[i] != 0.0) || (isComplex && (pi[i] != 0.0))) {
                        if (isComplex) {
                            si[k] = pi[i];
                        }
                        sr[k] = pr[i];
                        irs[k] = i;
                        k++;
                    }
                }
                pr += m;
                pi += m;
            }
        }
    }
    jcs[n] = k;
    plhs[0] = res;
}
//=============================================================================
