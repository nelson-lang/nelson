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
    void*
    mxGetData(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetData(mxArray* pm, void* pa);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
