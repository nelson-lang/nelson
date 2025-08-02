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
    char*
    mxArrayToString(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    char*
    mxArrayToUTF8String(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsChar(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxChar*
    mxGetChars(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxGetString(const mxArray* pm, char* str, mwSize strlen);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateString(const char* str);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateCharMatrixFromStrings(mwSize m, const char** str);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateCharArray(mwSize ndim, const mwSize* dims);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateStringFromNChars(const char* str, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxGetNChars(const mxArray* pa, char* buf, mwSize nChars);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
