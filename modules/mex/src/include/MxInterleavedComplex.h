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
