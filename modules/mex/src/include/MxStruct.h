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
#include "MxTypes.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateStructArray(mwSize ndim, const mwSize* dims, int nfields, const char** fieldnames);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateStructMatrix(mwSize m, mwSize n, int nfields, const char** fieldnames);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsStruct(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxGetNumberOfFields(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxGetFieldByNumber(const mxArray* pm, mwIndex index, int fieldnumber);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetField(mxArray* pm, mwIndex index, const char* fieldname, mxArray* pvalue);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetFieldByNumber(mxArray* pm, mwIndex index, int fieldnumber, mxArray* pvalue);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxGetFieldNumber(const mxArray* pm, const char* fieldname);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxAddField(mxArray* pm, const char* fieldname);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxRemoveField(mxArray* pm, int fieldnumber);
    //=============================================================================
    NLSMEX_IMPEXP
    const char*
    mxGetFieldNameByNumber(const mxArray* pm, int fieldnumber);
//=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
