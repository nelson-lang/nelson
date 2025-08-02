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
#include "MxTypes.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    double
    mxGetScalar(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsDouble(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsSingle(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPrSeparatedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPrInterleavedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetPr(mxArray* pm, double* pr);
    //=============================================================================
#if MX_HAS_INTERLEAVED_COMPLEX
#define mxGetPr mxGetPrInterleavedComplex
#else
#define mxGetPr mxGetPrSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
