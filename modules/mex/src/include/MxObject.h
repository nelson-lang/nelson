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
    bool mxIsOpaque(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsObject(const mxArray* pa);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
