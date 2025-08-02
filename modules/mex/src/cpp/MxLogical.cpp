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
bool
mxIsLogical(const mxArray* pm)
{
    if (pm != nullptr) {
        return pm->classID == mxLOGICAL_CLASS;
    }
    return false;
}
//=============================================================================
mxArray*
mxCreateLogicalScalar(mxLogical value)
{
    mxArray* ret = mxCreateLogicalMatrix(1, 1);
    ((mxLogical*)ret->realdata)[0] = value;
    return ret;
}
//=============================================================================
mxArray*
mxCreateLogicalMatrix(mwSize m, mwSize n)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateLogicalArray(2, dims);
}
//=============================================================================
mxArray*
mxCreateLogicalArray(mwSize ndim, const mwSize* dims)
{
    return mxAllocateRealArray(ndim, dims, sizeof(mxLogical), mxLOGICAL_CLASS);
}
//=============================================================================
mxLogical*
mxGetLogicals(const mxArray* array_ptr)
{
    if (array_ptr != nullptr) {
        return (mxLogical*)array_ptr->realdata;
    }
    return nullptr;
}
//=============================================================================
bool
mxIsLogicalScalar(const mxArray* array_ptr)
{
    return mxIsScalar(array_ptr) && mxIsLogical(array_ptr);
}
//=============================================================================
bool
mxIsLogicalScalarTrue(const mxArray* array_ptr)
{
    if (mxIsLogicalScalar(array_ptr)) {
        mxLogical logicalValue = array_ptr->realdata;
        return logicalValue == true;
    }
    return false;
}
//=============================================================================
