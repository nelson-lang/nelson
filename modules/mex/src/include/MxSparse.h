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
    bool
    mxIsSparse(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseInterleavedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseSeparatedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseLogicalMatrix(mwSize m, mwSize n, mwSize nzmax);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex*
    mxGetJc(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex*
    mxGetIr(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwSize
    mxGetNzmax(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetNzmax(mxArray* pm, mwSize nzmax);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetIr(mxArray* pm, mwIndex* ir);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetJc(mxArray* pm, mwIndex* ir);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxCreateSparse mxCreateSparseInterleavedComplex
#else
#define mxCreateSparse mxCreateSparseSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
