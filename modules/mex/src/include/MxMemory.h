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
#include "MxTypes.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxMalloc(mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxCalloc(mwSize n, mwSize size);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxRealloc(void* ptr, mwSize size);
    //=============================================================================
    NLSMEX_IMPEXP void
    mxFree(void* ptr);
    //=============================================================================
    NLSMEX_IMPEXP bool
    mxIsRegisteredPointer(void* ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxDestroyArray(mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mexMakeArrayPersistent(mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mexMakeMemoryPersistent(void* ptr);
    //=============================================================================
    NLSMEX_IMPEXP void
    mexFreeAllRegisteredPointer();
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
