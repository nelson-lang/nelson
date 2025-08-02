//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
static std::set<void*> persistentMxPointers;
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
    if (ptr != nullptr && !mxIsRegisteredPointer(ptr)) {
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
static bool
isPersistentMemory(void* ptr)
{
    if (ptr != nullptr) {
        std::set<void*>::iterator it;
        it = persistentMxPointers.find(ptr);
        return it != persistentMxPointers.end();
    }
    return false;
}
//=============================================================================
void
mexFreeAllRegisteredPointer()
{
    for (auto ptr : registeredMxPointers) {
        if (!isPersistentMemory(ptr)) {
            mxFree(ptr);
        }
    }
    registeredMxPointers.clear();
}
//=============================================================================
void
mxFree(void* ptr)
{
    if (ptr != nullptr) {
        if (mxIsRegisteredPointer(ptr)) {
            if (!isPersistentMemory(ptr)) {
                deRegisterMexPointer(ptr);
                free(ptr);
            }
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
    void* tmp = realloc(ptr, size);

    if (tmp != nullptr) {
        ptr = tmp;
        registerMexPointer(ptr);
    }
    return ptr;
}
//=============================================================================
void
mxDestroyArray(mxArray* pm)
{
    if (pm != nullptr) {
        if (pm->persistentmemory) {
            return;
        }
        if (pm->classID == mxCELL_CLASS) {
            auto** gp = (mxArray**)pm->realdata;
            size_t L = mxGetNumberOfElements(pm);
            for (size_t i = 0; i < L; i++) {
                mxArray* p = gp[i];
                mxDestroyArray(p);
            }
            pm->realdata = nullptr;
        }
        if (pm->classID == mxOBJECT_CLASS) {
            auto* ptr = (Nelson::ArrayOf*)pm->ptr;
            delete ptr;
            pm->ptr = nullptr;
        }
        if (pm->classID == mxSTRUCT_CLASS) {
            auto* ptr = (Nelson::ArrayOf*)pm->ptr;
            delete ptr;
            pm->ptr = nullptr;
        }
        if (pm->issparse) {
            mxFree(pm->Jc);
            mxFree(pm->Ir);
        }
        mxFree(pm->realdata);
        pm->realdata = nullptr;
        mxFree(pm->imagdata);
        pm->imagdata = nullptr;
        free(pm->dims);
        pm->dims = nullptr;
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
    case mxOBJECT_CLASS: {
        auto* inPtr = (Nelson::ArrayOf*)in->ptr;
        ret = mxNewArray();
        if (ret != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(*inPtr, num_dim);
            ret->number_of_dims = num_dim;
            ret->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            ret->classID = mxOBJECT_CLASS;
            ret->interleavedcomplex = in->interleavedcomplex;
            ret->issparse = in->issparse;
            ret->iscomplex = in->iscomplex;
            ret->imagdata = nullptr;
            ret->realdata = nullptr;
            auto* ptr = new Nelson::ArrayOf(*inPtr);
            ptr->ensureSingleOwner();
            ret->ptr = (uint64_t*)ptr;
            ret->Ir = nullptr;
            ret->Jc = nullptr;
            ret->nzmax = (mwSize)0;
            ret->nIr = (mwSize)0;
            ret->nJc = (mwSize)0;
        }
    } break;
    case mxCELL_CLASS: {
        ret = mxNewArray();
        if (ret != nullptr) {
            mwSize n = countElements(in->number_of_dims, in->dims);
            ret->number_of_dims = in->number_of_dims;
            ret->dims = copyDims(in->number_of_dims, in->dims);
            ret->classID = mxCELL_CLASS;
            ret->interleavedcomplex = in->interleavedcomplex;
            ret->issparse = false;
            ret->iscomplex = false;
            ret->imagdata = nullptr;
            ret->realdata = mxCalloc(
                countElements(in->number_of_dims, in->dims), sizeFromClass(mxCELL_CLASS));
            ret->ptr = nullptr;
            ret->Ir = nullptr;
            ret->Jc = nullptr;
            ret->nzmax = (mwSize)0;
            ret->nIr = (mwSize)0;
            ret->nJc = (mwSize)0;
            if (ret->realdata && in->realdata) {
                auto** h = (mxArray**)in->realdata;
                for (size_t i = 0; i < n; i++) {
                    ((mxArray**)ret->realdata)[i] = mxDuplicateArray(h[i]);
                }
            }
        }
    } break;
    case mxSTRUCT_CLASS: {
        auto* inPtr = (Nelson::ArrayOf*)in->ptr;
        ret = mxNewArray();
        if (ret != nullptr) {
            mwSize num_dim;
            mwSize* dim_vec = GetDimensions(*inPtr, num_dim);
            ret->number_of_dims = num_dim;
            ret->dims = copyDims(num_dim, dim_vec);
            free(dim_vec);
            ret->classID = mxSTRUCT_CLASS;
            ret->interleavedcomplex = in->interleavedcomplex;
            ret->issparse = false;
            ret->iscomplex = false;
            ret->imagdata = nullptr;
            ret->realdata = nullptr;
            auto* ptr = new Nelson::ArrayOf(*inPtr);
            ptr->ensureSingleOwner();
            ret->ptr = (uint64_t*)ptr;
            ret->Ir = nullptr;
            ret->Jc = nullptr;
            ret->nzmax = (mwSize)0;
            ret->nIr = (mwSize)0;
            ret->nJc = (mwSize)0;
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
        if (in->issparse) {
            ret = mxNewArray();
            ret->classID = in->classID;
            ret->interleavedcomplex = in->interleavedcomplex;
            ret->iscomplex = in->iscomplex;
            ret->issparse = in->issparse;
            ret->ptr = nullptr;
            ret->number_of_dims = in->number_of_dims;
            ret->dims = copyDims(in->number_of_dims, in->dims);
            ret->nzmax = in->nzmax;
            ret->nIr = in->nIr;
            ret->nJc = in->nJc;
            ret->Ir = (mwIndex*)mxCalloc(in->nIr, sizeof(mwIndex));
            if (ret->Ir != nullptr) {
                memcpy(ret->Ir, in->Ir, sizeof(mwSize) * in->nIr);
            }
            ret->Jc = (mwIndex*)mxCalloc(in->nJc, sizeof(mwIndex));
            if (ret->Jc != nullptr) {
                memcpy(ret->Jc, in->Jc, sizeof(mwSize) * in->nJc);
            }
            if (in->interleavedcomplex) {
                if (in->iscomplex) {
                    ret->realdata = mxCalloc(in->nIr, sizeof(mxComplexDouble));
                    if (ret->realdata) {
                        memcpy(ret->realdata, in->realdata, sizeof(mxComplexDouble) * in->nIr);
                    }
                    ret->imagdata = nullptr;
                } else {
                    ret->realdata = mxCalloc(in->nIr, sizeFromClass(in->classID));
                    if (ret->realdata != nullptr) {
                        memcpy(ret->realdata, in->realdata, sizeFromClass(in->classID) * in->nIr);
                    }
                    ret->imagdata = nullptr;
                }
            } else {
                ret->realdata = mxCalloc(in->nIr, sizeFromClass(in->classID));
                if (ret->realdata != nullptr) {
                    memcpy(ret->realdata, in->realdata, sizeFromClass(in->classID) * in->nIr);
                }
                if (in->iscomplex) {
                    ret->imagdata = mxCalloc(in->nIr, sizeFromClass(in->classID));
                    if (ret->imagdata != nullptr) {
                        memcpy(ret->imagdata, in->imagdata, sizeFromClass(in->classID) * in->nIr);
                    }
                } else {
                    ret->imagdata = nullptr;
                }
            }
        } else {
            if (in->iscomplex) {
                if (in->interleavedcomplex) {
                    ret = mxAllocateInterleavedComplexArray(
                        in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
                    if (ret != nullptr) {
                        memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L * 2);
                        ret->imagdata = nullptr;
                    }
                } else {
                    ret = mxAllocateSeparatedComplexArray(
                        in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
                    if (ret != nullptr) {
                        memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
                        memcpy(ret->imagdata, in->imagdata, mxGetElementSize(in) * L);
                    }
                }
            } else {
                ret = mxAllocateRealArray(
                    in->number_of_dims, in->dims, sizeFromClass(in->classID), in->classID);
                if (ret != nullptr) {
                    memcpy(ret->realdata, in->realdata, mxGetElementSize(in) * L);
                }
            }
        }
    } break;
    default: {
        mexErrMsgTxt("C MEX type not managed.");
    } break;
    }
    return ret;
}
//=============================================================================
void
mexMakeArrayPersistent(mxArray* pm)
{
    if (pm != nullptr) {
        pm->persistentmemory = true;
    }
}
//=============================================================================
NLSMEX_IMPEXP
void
mexMakeMemoryPersistent(void* ptr)
{
    if (ptr != nullptr) {
        persistentMxPointers.insert(ptr);
    }
}
//=============================================================================
