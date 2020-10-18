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
