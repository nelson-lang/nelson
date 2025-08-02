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
#include "ArrayOf.hpp"
#include "matrix.h"
//=============================================================================
mxArray*
mxNewArray();
//=============================================================================
mxArray*
mxAllocateRealArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized = true);
//=============================================================================
mxArray*
mxAllocateSeparatedComplexArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized = true);
//=============================================================================
mxArray*
mxAllocateInterleavedComplexArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized = true);
//=============================================================================
mwSize
countElements(mwSize ndim, const mwSize* dims);
//=============================================================================
mwSize*
copyDims(mwSize ndim, const mwSize* dims);
//=============================================================================
mwSize
sizeFromClass(mxClassID classid);
//=============================================================================
mwSize*
GetDimensions(const Nelson::ArrayOf& array, mwSize& numdims);
//=============================================================================
