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
#include <set>
#include <cstring>
#include "mex.h"
#include "ArrayOf.hpp"
#include "MxHelpers.hpp"
//=============================================================================
static std::set<void*> registeredMxPointers;
//=============================================================================
bool
mxIsRegisteredPointer(void* ptr)
{
    std::set<void*>::iterator it;
    it = registeredMxPointers.find(ptr);
    return it != registeredMxPointers.end();
}
//=============================================================================
static void
registerMexPointer(void* ptr)
{
    if (ptr != nullptr) {
        registeredMxPointers.insert(ptr);
    }
}
//=============================================================================
static void
deRegisterMexPointer(void* ptr)
{
    if (ptr != nullptr) {
        registeredMxPointers.erase(ptr);
    }
}
//=============================================================================
void*
mxCalloc(mwSize n, mwSize size)
{
    void* p = calloc(n, size);
    registerMexPointer(p);
    return p;
}
//=============================================================================
void*
mxMalloc(mwSize n)
{
    void* p = malloc(n);
    registerMexPointer(p);
    return p;
}
//=============================================================================
void
mxFree(void* ptr)
{
    if (ptr != nullptr) {
        if (mxIsRegisteredPointer(ptr)) {
            deRegisterMexPointer(ptr);
            free(ptr);
        }
    }
}
//=============================================================================
void*
mxRealloc(void* ptr, mwSize size)
{
    if (ptr != nullptr) {
        deRegisterMexPointer(ptr);
    }
    ptr = realloc(ptr, size);

    if (ptr != nullptr) {
        registerMexPointer(ptr);
    }
    return ptr;
}
//=============================================================================
void
mxDestroyArray(mxArray* pm)
{
    if (pm != nullptr) {
        if (pm->classID == mxCELL_CLASS) {
            auto** gp = (mxArray**)pm->realdata;
            size_t L = mxGetNumberOfElements(pm);
            for (size_t i = 0; i < L; i++) {
                mxArray* p = gp[i];
                mxDestroyArray(p);
            }
        }
        if (pm->classID == mxSTRUCT_CLASS) {
            auto* ptr = (Nelson::ArrayOf*)pm->ptr;
            delete ptr;
            pm->ptr = nullptr;
        }
        mxFree(pm->realdata);
        mxFree(pm->imagdata);
        mxFree(pm);
    }
}
//=============================================================================
mxArray*
mxDuplicateArray(const mxArray* in)
{
    if (in == nullptr) {
        return nullptr;
    }
    size_t L = mxGetNumberOfElements(in);
    mxArray* ret = nullptr;
    switch (in->classID) {
    case mxCELL_CLASS: {
        ret = mxAllocateRealArray(
            in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
        auto** g = (mxArray**)ret->realdata;
        auto** h = (mxArray**)in->realdata;
        for (size_t i = 0; i < L; i++) {
            g[i] = mxDuplicateArray(h[i]);
        }
    } break;
    case mxSTRUCT_CLASS: {
        auto* inPtr = (Nelson::ArrayOf*)in->ptr;
        ret = (mxArray*)malloc(sizeof(mxArray));
        if (ret != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(*inPtr, num_dim);
            ret->number_of_dims = num_dim;
            ret->dims = dim_vec;
            ret->classID = mxSTRUCT_CLASS;
            ret->interleavedcomplex = in->interleavedcomplex;
            ret->issparse = false;
            ret->iscomplex = false;
            ret->imagdata = nullptr;
            ret->realdata = nullptr;
            auto* ptr = new Nelson::ArrayOf(*inPtr);
            ptr->ensureSingleOwner();
            ret->ptr = (uint64_t*)ptr;
        }
    } break;
    case mxLOGICAL_CLASS:
    case mxCHAR_CLASS:
    case mxDOUBLE_CLASS:
    case mxSINGLE_CLASS:
    case mxINT8_CLASS:
    case mxUINT8_CLASS:
    case mxINT16_CLASS:
    case mxUINT16_CLASS:
    case mxINT32_CLASS:
    case mxUINT32_CLASS:
    case mxINT64_CLASS:
    case mxUINT64_CLASS: {
        if (in->iscomplex) {
            if (in->interleavedcomplex) {
                ret = mxAllocateInterleavedComplexArray(
                    in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
                memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L * 2);
                ret->imagdata = nullptr;
            } else {
                ret = mxAllocateSeparatedComplexArray(
                    in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
                memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
                memcpy(ret->imagdata, in->imagdata, mxGetElementSize(in) * L);
            }
        } else {
            ret = mxAllocateRealArray(
                in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
            memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
        }
        return ret;
    } break;
    default: {
        mexErrMsgTxt("C MEX type not managed.");
    } break;
    }
    return ret;
}
//=============================================================================
