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
#include "mex.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsLogical(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalScalar(mxLogical value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalMatrix(mwSize m, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalArray(mwSize ndim, const mwSize* dims);
    //=============================================================================
    NLSMEX_IMPEXP
    mxLogical*
    mxGetLogicals(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsLogicalScalar(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsLogicalScalarTrue(const mxArray* array_ptr);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
