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
    mxDouble*
    mxGetPiSeparatedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetPiSeparatedComplex(mxArray* pm, double* pr);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPiInterleavedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetPiInterleavedComplex(mxArray* pm, double* pr);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxGetImagDataSeparatedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetImagDataSeparatedComplex(mxArray* pm, void* pi);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxGetPi mxGetPiInterleavedComplex
#define mxSetPi mxSetPiInterleavedComplex
#else
#define mxGetPi mxGetPiSeparatedComplex
#define mxSetPi mxSetPiSeparatedComplex
#define mxGetImagData mxGetImagDataSeparatedComplex
#define mxSetImagData mxSetImagDataSeparatedComplex
#endif
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
