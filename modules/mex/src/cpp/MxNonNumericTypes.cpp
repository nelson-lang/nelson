//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
void*
mxGetData(const mxArray* pm)
{
    if (pm != nullptr) {
        if (pm->classID == mxSTRUCT_CLASS || pm->classID == mxOBJECT_CLASS) {
            return pm->ptr;
        }
        return pm->realdata;
    }
    return nullptr;
}
//=============================================================================
NLSMEX_IMPEXP
void
mxSetData(mxArray* pm, void* pa)
{
    pm->realdata = pa;
}
//=============================================================================
