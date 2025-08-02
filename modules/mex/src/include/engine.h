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
#include "matrix.h"
#include "nlsMex_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    struct engine_tag
    {
        void* child;
        bool isSingleUse;
    };
    //=============================================================================
    typedef struct engine_tag Engine;
    //=============================================================================
    /* Execute Nelson statement */
    NLSMEX_IMPEXP int
    engEvalString(Engine* ep, const char* string);
    //=============================================================================
    /* Start Nelson process for single use */
    NLSMEX_IMPEXP Engine*
    engOpenSingleUse(const char* startcmd, void* reserved, int* retstatus);
    //=============================================================================
    /* SetVisible */
    NLSMEX_IMPEXP int
    engSetVisible(Engine* ep, bool newVal);
    //=============================================================================
    /* GetVisible */
    NLSMEX_IMPEXP int
    engGetVisible(Engine* ep, bool* bVal);
    //=============================================================================
    /* Start Nelson process */
    NLSMEX_IMPEXP Engine*
    engOpen(const char* startcmd);
    //=============================================================================
    /* Close down Nelson server */
    NLSMEX_IMPEXP int
    engClose(Engine* ep);
    //=============================================================================
    /* Get a variable with the specified name from Nelson's workspace (interleaved) */
    NLSMEX_IMPEXP mxArray*
    engGetVariableInterleavedComplex(Engine* ep, const char* name);
    //=============================================================================
    /* Get a variable with the specified name from Nelson's workspace (complex) */
    NLSMEX_IMPEXP mxArray*
    engGetVariableSeparatedComplex(Engine* ep, const char* name);
    //=============================================================================
    /* Put a variable into Nelson's workspace with the specified name */
    NLSMEX_IMPEXP int
    engPutVariable(Engine* ep, const char* var_name, const mxArray* ap);
    //=============================================================================
    /* register a buffer to hold Nelson text output */
    NLSMEX_IMPEXP int
    engOutputBuffer(Engine* ep, char* buffer, int buflen);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define engGetVariable engGetVariableInterleavedComplex
#else
#define engGetVariable engGetVariableSeparatedComplex
#endif
//=============================================================================
