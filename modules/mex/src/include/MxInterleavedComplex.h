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
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexDouble*
    mxGetComplexDoublesInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetDoublesInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetDoublesInterleavedComplex(mxArray* pa, mxDouble* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexDoublesInterleavedComplex(mxArray* pa, mxComplexDouble* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxComplexSingle*
    mxGetComplexSinglesInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    mxSingle*
    mxGetSinglesInterleavedComplex(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetSinglesInterleavedComplex(mxArray* pa, mxSingle* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetComplexSinglesInterleavedComplex(mxArray* pa, mxComplexSingle* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxMakeArrayRealInterleavedComplex(mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxMakeArrayComplexInterleavedComplex(mxArray* pa);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxGetComplexDoubles mxGetComplexDoublesInterleavedComplex
#define mxGetDoubles mxGetDoublesInterleavedComplex
#define mxSetDoubles mxSetDoublesInterleavedComplex
#define mxSetComplexDoubles mxSetComplexDoublesInterleavedComplex
#define mxGetComplexSingles mxGetComplexSinglesInterleavedComplex
#define mxGetSingles mxGetSinglesInterleavedComplex
#define mxSetSingles mxSetSinglesInterleavedComplex
#define mxSetComplexSingles mxSetComplexSinglesInterleavedComplex
#define mxMakeArrayReal mxMakeArrayRealInterleavedComplex
#define mxMakeArrayComplex mxMakeArrayComplexInterleavedComplexs
#endif
    //=============================================================================

//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
