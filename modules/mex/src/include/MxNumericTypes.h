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
#include "MxCall.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
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
#ifdef __cplusplus
}
#endif
//=============================================================================