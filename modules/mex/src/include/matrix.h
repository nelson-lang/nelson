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
#include "MxAsserts.h"
#include "MxAttributes.h"
#include "MxCell.h"
#include "MxCharacter.h"
#include "MxConstants.h"
#include "MxLogical.h"
#include "MxMemory.h"
#include "MxNonComplexFloat.h"
#include "MxNumericTypes.h"
#include "MxStruct.h"
#include "MxObject.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxDuplicateArray(const mxArray* in);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetNumberOfElements(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericArray(
        mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    char*
    mxArrayToString(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    char*
    mxArrayToUTF8String(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleMatrix(mwSize m, mwSize n, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsNumeric(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwSize
    mxGetNumberOfDimensions(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetElementSize(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    const mwSize*
    mxGetDimensions(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetDimensions(mxArray* pm, const mwSize* dims, mwSize ndim);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex
    mxCalcSingleSubscript(const mxArray* pm, mwSize nsubs, mwIndex* subs);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetM(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetN(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetM(mxArray* pm, mwSize m);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetN(mxArray* pm, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsEmpty(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsFromGlobalWS(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleScalar(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericMatrix(mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericMatrix(size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericArray(
        size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsScalar(const mxArray* array_ptr);
    //=============================================================================

    NLSMEX_IMPEXP
    bool
    mxIsChar(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxChar*
    mxGetChars(const mxArray* array_ptr);
    //=============================================================================

#ifdef __cplusplus
}
#endif
//=============================================================================
