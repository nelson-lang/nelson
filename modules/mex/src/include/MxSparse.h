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
    bool
    mxIsSparse(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseInterleavedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseSeparatedComplex(mwSize m, mwSize n, mwSize nzmax, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateSparseLogicalMatrix(mwSize m, mwSize n, mwSize nzmax);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex*
    mxGetJc(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex*
    mxGetIr(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwSize
    mxGetNzmax(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetNzmax(mxArray* pm, mwSize nzmax);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetIr(mxArray* pm, mwIndex* ir);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetJc(mxArray* pm, mwIndex* ir);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxCreateSparse mxCreateSparseInterleavedComplex
#else
#define mxCreateSparse mxCreateSparseMatrixSeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
