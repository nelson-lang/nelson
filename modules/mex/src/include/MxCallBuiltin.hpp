//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsMex_exports.h"
#include "mex.h"
#include "ArrayOf.hpp"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxCallBuiltin(void* fptr, const Nelson::ArrayOfVector& argIn, int nargout,
        Nelson::ArrayOfVector& argOut, bool interleavedComplex);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
