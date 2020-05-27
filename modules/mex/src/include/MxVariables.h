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
