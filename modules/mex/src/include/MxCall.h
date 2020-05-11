//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
    /* private function */
    NLSMEX_IMPEXP
    void
    mexSetEvaluator(void* eval);
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
