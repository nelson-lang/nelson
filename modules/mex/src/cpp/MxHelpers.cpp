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
#include <cstdlib>
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
mxArray*
mxNewArray()
{
    return (mxArray*)malloc(sizeof(mxArray));
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
    case mxCELL_CLASS:
        return sizeof(mxArray*);
    case mxSTRUCT_CLASS:
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
        return sizeof(mxInt8);
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
    numdims = (int)array.getDimensions().getLength();
    auto* dim_vec = new mwSize[numdims];
    for (mwSize i = 0; i < numdims; i++) {
        dim_vec[i] = array.getDimensions()[i];
    }
    return dim_vec;
}
//=============================================================================
