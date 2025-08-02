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
    mxArray* res = NULL;
    if (nlhs > 1) {
        mexErrMsgTxt("Wrong number of output argument.");
    }
    if (nrhs != 1) {
        mexErrMsgTxt("Wrong number of input argument.");
    }
    if (mxIsInt8(prhs[0])) {
        res = mxCreateString("int8");
    } else if (mxIsInt16(prhs[0])) {
        res = mxCreateString("int16");
    } else if (mxIsInt32(prhs[0])) {
        res = mxCreateString("int32");
    } else if (mxIsInt64(prhs[0])) {
        res = mxCreateString("int64");
    } else if (mxIsUint8(prhs[0])) {
        res = mxCreateString("uint8");
    } else if (mxIsUint16(prhs[0])) {
        res = mxCreateString("uint16");
    } else if (mxIsUint32(prhs[0])) {
        res = mxCreateString("uint32");
    } else if (mxIsUint64(prhs[0])) {
        res = mxCreateString("uint64");
    } else {
        res = mxCreateString("not integer");
    }
    plhs[0] = res;
}
//=============================================================================
