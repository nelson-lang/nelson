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
    void
    mexPrintAssertion(const char* test, const char* fname, int linenum, const char* message);
    //=============================================================================
    /**
     * Check assertion value for debugging purposes
     * @param test Value of assertion
     * @param message why assert failed
     */
#define mxAssert(test, message)                                                                    \
    ((test) ? (void)0 : mexPrintAssertion(#test, __FILE__, __LINE__, message))
    //=============================================================================
    /**
     * Check assertion value without printing assertion text
     * @param test Value of assertion
     * @param message why assert failed
     */

#define mxAssertS(test, message)                                                                   \
    ((test) ? (void)0 : mexPrintAssertion("", __FILE__, __LINE__, message))
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
