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
    bool
    mxIsClass(const mxArray* pm, const char* classname);
    //=============================================================================
    NLSMEX_IMPEXP
    mxClassID
    mxGetClassID(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    const char*
    mxGetClassName(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetClassName(mxArray* array_ptr, const char* classname);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxGetProperty(const mxArray* pa, mwIndex index, const char* propname);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetProperty(mxArray* pa, mwIndex index, const char* propname, const mxArray* value);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsFunctionHandle(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsOpaque(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsObject(const mxArray* pa);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
