//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string.h>
#include <mex.h>
//=============================================================================
#define TOTAL_ELEMENTS 4
//=============================================================================
void
mexFunction(int nlhs, mxArray* plhs[], int nrhs, const mxArray* prhs[])
{
    const char* strings[3];
    strings[0] = "value";
    strings[1] = "another value";
    strings[2] = "another value longest";
    mxArray* pOut = mxCreateCharMatrixFromStrings(3, strings);
    plhs[0] = pOut;
}
//=============================================================================
