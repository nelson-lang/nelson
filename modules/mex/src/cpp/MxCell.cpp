//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "mex.h"
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
mxArray*
mxCreateCellArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(void*), mxCELL_CLASS);
}
//=============================================================================
mxArray*
mxCreateCellMatrix(mwSize m, mwSize n)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateCellArray(2, dims);
}
//=============================================================================
bool
mxIsCell(const mxArray* pm)
{
    if (pm != nullptr) {
        return (pm->classID == mxCELL_CLASS);
    }
    return false;
}
//=============================================================================
mxArray*
mxGetCell(const mxArray* pm, mwIndex index)
{
    if (pm != nullptr) {
        return (((mxArray**)pm->realdata)[index]);
    }
    return nullptr;
}
//=============================================================================
void
mxSetCell(mxArray* pm, mwIndex index, mxArray* value)
{
    if (pm == nullptr) {
        return;
    }
    if (!mxIsCell(pm)) {
        return;
    }
    ((mxArray**)pm->realdata)[index] = value;
}
//=============================================================================
