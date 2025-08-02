//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsMex_exports.h"
#include "mex.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mexCallMATLABSeparatedComplex(
        int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mexCallMATLABInterleavedComplex(
        int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mexCallMATLABWithTrapSeparatedComplex(
        int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mexCallMATLABWithTrapInterleavedComplex(
        int nlhs, mxArray* plhs[], int nrhs, mxArray* prhs[], const char* functionName);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mexEvalString(const char* command);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mexEvalStringWithTrapSeparatedComplex(const char* command);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mexEvalStringWithTrapInterleavedComplex(const char* command);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mexCallMATLAB mexCallMATLABInterleavedComplex
#define mexCallMATLABWithTrap mexCallMATLABWithTrapInterleavedComplex
#define mexEvalStringWithTrap mexEvalStringWithTrapInterleavedComplex
#else
#define mexCallMATLAB mexCallMATLABSeparatedComplex
#define mexCallMATLABWithTrap mexCallMATLABWithTrapSeparatedComplex
#define mexEvalStringWithTrap mexEvalStringWithTrapSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
