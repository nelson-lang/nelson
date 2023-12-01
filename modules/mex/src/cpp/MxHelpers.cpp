//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cstdlib>
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
mxArray*
mxNewArray()
{
    mxArray* res = (mxArray*)mxMalloc(sizeof(mxArray));
    res->dims = nullptr;
    res->imagdata = nullptr;
    res->realdata = nullptr;
    res->ptr = nullptr;
    res->interleavedcomplex = false;
    res->Ir = nullptr;
    res->Jc = nullptr;
    res->nIr = 0;
    res->nJc = 0;
    res->iscomplex = false;
    res->issparse = false;
    res->classID = mxUNKNOWN_CLASS;
    res->number_of_dims = 0;
    res->nzmax = 0;
    res->persistentmemory = false;
    return res;
}
//=============================================================================
mxArray*
mxAllocateRealArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized)
{
    mxArray* ret = mxNewArray();
    if (ret != nullptr) {
        ret->classID = classID;
        ret->number_of_dims = ndim;
        ret->dims = copyDims(ndim, dims);
        ret->issparse = false;
        ret->iscomplex = false;
        if (initialized) {
            ret->realdata = mxCalloc(countElements(ndim, dims), size);

        } else {
            ret->realdata = mxMalloc(countElements(ndim, dims));
        }
        ret->imagdata = nullptr;
        ret->Ir = nullptr;
        ret->Jc = nullptr;
        ret->nzmax = 0;
        ret->nIr = 0;
        ret->nJc = 0;
    }
    return ret;
}
//=============================================================================
mxArray*
mxAllocateSeparatedComplexArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized)
{
    mxArray* ret = mxNewArray();
    if (ret != nullptr) {
        ret->classID = classID;
        ret->number_of_dims = ndim;
        ret->dims = copyDims(ndim, dims);
        ret->issparse = false;
        ret->iscomplex = true;
        ret->interleavedcomplex = false;
        if (initialized) {
            ret->realdata = mxCalloc(countElements(ndim, dims), size);
            ret->imagdata = mxCalloc(countElements(ndim, dims), size);
        } else {
            ret->realdata = mxMalloc(countElements(ndim, dims));
            ret->imagdata = mxMalloc(countElements(ndim, dims));
        }
        ret->Ir = nullptr;
        ret->Jc = nullptr;
        ret->nzmax = 0;
        ret->nIr = 0;
        ret->nJc = 0;
    }
    return ret;
}
//=============================================================================
mxArray*
mxAllocateInterleavedComplexArray(
    mwSize ndim, const mwSize* dims, size_t size, mxClassID classID, bool initialized)
{
    mxArray* ret = mxNewArray();
    if (ret != nullptr) {
        ret->classID = classID;
        ret->interleavedcomplex = true;
        ret->number_of_dims = ndim;
        ret->dims = copyDims(ndim, dims);
        ret->issparse = false;
        ret->iscomplex = true;
        if (initialized) {
            ret->realdata = mxCalloc(countElements(ndim, dims) * 2, size);
            ret->imagdata = nullptr;
        } else {
            ret->realdata = mxMalloc(countElements(ndim, dims) * 2);
            ret->imagdata = nullptr;
        }
        ret->Ir = nullptr;
        ret->Jc = nullptr;
        ret->nzmax = 0;
        ret->nIr = 0;
        ret->nJc = 0;
    }
    return ret;
}
//=============================================================================
mwSize
countElements(mwSize ndim, const mwSize* dims)
{
    mwSize count = 1;
    for (mwSize i = 0; i < ndim; i++) {
        count *= dims[i];
    }
    return count;
}
//=============================================================================
mwSize*
copyDims(mwSize ndim, const mwSize* dims)
{
    auto* p = (mwSize*)malloc(sizeof(mwSize) * ndim);
    if (p != nullptr) {
        for (mwSize i = 0; i < ndim; i++) {
            p[i] = dims[i];
        }
    }
    return p;
}
//=============================================================================
mwSize
sizeFromClass(mxClassID classid)
{
    switch (classid) {
    case mxSTRUCT_CLASS:
    case mxCELL_CLASS:
        return sizeof(mxArray*);
    case mxLOGICAL_CLASS:
        return sizeof(mxLogical);
    case mxCHAR_CLASS:
        return sizeof(mxChar);
    case mxDOUBLE_CLASS:
        return sizeof(mxDouble);
    case mxSINGLE_CLASS:
        return sizeof(mxSingle);
    case mxINT8_CLASS:
        return sizeof(mxInt8);
    case mxUINT8_CLASS:
        return sizeof(mxUint8);
    case mxINT16_CLASS:
        return sizeof(mxInt16);
    case mxUINT16_CLASS:
        return sizeof(mxUint16);
    case mxINT32_CLASS:
        return sizeof(mxInt32);
    case mxUINT32_CLASS:
        return sizeof(mxUint32);
    case mxINT64_CLASS:
        return sizeof(mxInt64);
    case mxUINT64_CLASS:
        return sizeof(mxUint64);
    default:
        return 0;
    }
}
//=============================================================================
mwSize*
GetDimensions(const Nelson::ArrayOf& array, mwSize& numdims)
{
    numdims = (mwSize)(array.nDims());
    auto* dim_vec = (mwSize*)malloc(sizeof(mwSize) * numdims);
    for (mwSize i = 0; i < numdims; i++) {
        dim_vec[i] = array.getDimensions()[i];
    }
    return dim_vec;
}
//=============================================================================
