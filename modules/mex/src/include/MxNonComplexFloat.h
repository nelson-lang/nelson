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
    double
    mxGetScalar(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsDouble(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsSingle(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPrSeparatedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPrInterleavedComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetPr(mxArray* pm, double* pr);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxGetData(const mxArray* pm);
    //=============================================================================
#if MX_HAS_INTERLEAVED_COMPLEX
#define mxGetPr mxGetPrInterleavedComplex
#else
#define mxGetPr mxGetPrSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
