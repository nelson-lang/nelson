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
    mxArray*
    mexGetVariableSeparatedComplex(const char* workspace, const char* varname);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mexGetVariableInterleavedComplex(const char* workspace, const char* varname);
    //=============================================================================
    NLSMEX_IMPEXP
    const mxArray*
    mexGetVariablePtrSeparatedComplex(const char* workspace, const char* varname);
    //=============================================================================
    NLSMEX_IMPEXP
    const mxArray*
    mexGetVariablePtrInterleavedComplex(const char* workspace, const char* varname);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mexPutVariable(const char* workspace, const char* varname, const mxArray* pm);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mexGetVariable mexGetVariableInterleavedComplex
#define mexGetVariablePtr mexGetVariablePtrInterleavedComplex
#else
#define mexGetVariable mexGetVariableSeparatedComplex
#define mexGetVariablePtr mexGetVariablePtrSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
