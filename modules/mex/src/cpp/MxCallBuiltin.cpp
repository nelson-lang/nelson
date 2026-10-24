//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4297)
#endif
//=============================================================================
#include <vector>
#include <stdexcept>
#include "MxCallBuiltin.hpp"
#include "ArrayOf.hpp"
#include "MxArrayOf.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "NelsonException.hpp"
#include "mex.h"
#include "MxHelpers.hpp"
//=============================================================================
using MexFuncPtr = void (*)(int, mxArray**, int, const mxArray**);
//=============================================================================
void
mxProtectPointerFromFreeAll(void* ptr);
void
mxUnprotectPointerFromFreeAll(void* ptr);
void
mxProtectArrayFromFreeAll(mxArray* pm);
void
mxUnprotectArrayFromFreeAll(mxArray* pm);
//=============================================================================
int
mxCallBuiltin(void* fptr, const Nelson::ArrayOfVector& argIn, int nargout,
    Nelson::ArrayOfVector& argOut, bool interleavedComplex)
{
    mxArray** mxArgsIn = nullptr;
    mxArray** mxArgsOut = nullptr;
    std::vector<mxArray*> defaultArgsOut;

    try {
        if (!argIn.empty()) {
            mxArgsIn = static_cast<mxArray**>(mxMalloc(sizeof(mxArray*) * argIn.size()));
        }
    } catch (const std::bad_alloc&) {
        Nelson::Error(
            L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }
    int nlhs = (int)argIn.size();
    int lhsCount = (nargout < 1) ? 1 : nargout;
    try {
        mxArgsOut = (mxArray**)mxMalloc(mwSize(sizeof(mxArray*) * lhsCount));
        defaultArgsOut.resize(lhsCount, nullptr);
        for (size_t i = 0; i < (size_t)lhsCount; ++i) {
            mxArgsOut[i] = mxNewArray();
            defaultArgsOut[i] = mxArgsOut[i];
            mxArgsOut[i]->interleavedcomplex = interleavedComplex;
            mxArgsOut[i]->classID = mxUNKNOWN_CLASS;
            mxArgsOut[i]->dims = nullptr;
            mxArgsOut[i]->number_of_dims = 0;
            mxArgsOut[i]->issparse = false;
            mxArgsOut[i]->iscomplex = false;
            mxArgsOut[i]->realdata = nullptr;
            mxArgsOut[i]->imagdata = nullptr;
            mxArgsOut[i]->ptr = nullptr;
            mxArgsOut[i]->nzmax = 0;
            mxArgsOut[i]->nIr = 0;
            mxArgsOut[i]->nJc = 0;
            mxArgsOut[i]->Jc = nullptr;
            mxArgsOut[i]->Ir = nullptr;
        }
    } catch (const std::bad_alloc&) {
        for (size_t i = 0; i < argIn.size(); i++) {
            mxDestroyArray(mxArgsIn[i]);
        }
        mxFree(mxArgsIn);
        mxArgsIn = nullptr;
        Nelson::Error(
            L"Nelson:outOfMemory", L"Memory allocation error... You may have run out of memory!");
    }

    if (mxArgsIn != nullptr) {
        for (size_t i = 0; i < argIn.size(); ++i) {
            mxArgsIn[i] = Nelson::ArrayOfToMxArray(argIn[i], interleavedComplex);
        }
    }
    mxProtectPointerFromFreeAll(mxArgsIn);
    mxProtectPointerFromFreeAll(mxArgsOut);
    if (mxArgsIn != nullptr) {
        for (size_t i = 0; i < argIn.size(); ++i) {
            mxProtectArrayFromFreeAll(mxArgsIn[i]);
        }
    }
    if (mxArgsOut != nullptr) {
        for (int i = 0; i < lhsCount; ++i) {
            mxProtectArrayFromFreeAll(mxArgsOut[i]);
        }
    }

    auto builtinPtr = (MexFuncPtr)fptr;

    try {
        builtinPtr(nargout, mxArgsOut, nlhs, (const mxArray**)mxArgsIn);
    } catch (const std::runtime_error& e) {
        if (mxArgsIn != nullptr) {
            for (size_t i = 0; i < argIn.size(); i++) {
                mxUnprotectArrayFromFreeAll(mxArgsIn[i]);
                mxDestroyArray(mxArgsIn[i]);
            }
            mxUnprotectPointerFromFreeAll(mxArgsIn);
            mxFree(mxArgsIn);
            mxArgsIn = nullptr;
        }
        if (mxArgsOut != nullptr) {
            for (int i = 0; i < lhsCount; i++) {
                mxUnprotectArrayFromFreeAll(mxArgsOut[i]);
                mxDestroyArray(mxArgsOut[i]);
            }
            mxUnprotectPointerFromFreeAll(mxArgsOut);
            mxFree(mxArgsOut);
            mxArgsOut = nullptr;
        }
        Nelson::Error(e.what());
    } catch (Nelson::NelsonException& e) {
        if (mxArgsIn != nullptr) {
            for (size_t i = 0; i < argIn.size(); i++) {
                mxUnprotectArrayFromFreeAll(mxArgsIn[i]);
                mxDestroyArray(mxArgsIn[i]);
            }
            mxUnprotectPointerFromFreeAll(mxArgsIn);
            mxFree(mxArgsIn);
            mxArgsIn = nullptr;
        }
        if (mxArgsOut != nullptr) {
            const int protectLhsCount = lhsCount;
            for (int i = 0; i < protectLhsCount; i++) {
                mxUnprotectArrayFromFreeAll(mxArgsOut[i]);
                mxDestroyArray(mxArgsOut[i]);
            }
            mxUnprotectPointerFromFreeAll(mxArgsOut);
            mxFree(mxArgsOut);
            mxArgsOut = nullptr;
        }
        throw e;
    }
    if (mxArgsOut != nullptr) {
        for (int i = 0; i < lhsCount; ++i) {
            if (i < defaultArgsOut.size() && defaultArgsOut[i] != nullptr
                && defaultArgsOut[i] != mxArgsOut[i]) {
                mxUnprotectArrayFromFreeAll(defaultArgsOut[i]);
                mxDestroyArray(defaultArgsOut[i]);
            }
        }
        mxUnprotectPointerFromFreeAll(mxArgsOut);
        bool noOutput = (lhsCount == 1) && (mxArgsOut[0]->classID == mxUNKNOWN_CLASS)
            && (mxArgsOut[0]->number_of_dims == 0);
        if (!noOutput) {
            for (int i = 0; i < lhsCount; i++) {
                mxUnprotectArrayFromFreeAll(mxArgsOut[i]);
                argOut.push_back(Nelson::MxArrayToArrayOf(mxArgsOut[i]));
                mxDestroyArray(mxArgsOut[i]);
            }
        } else {
            mxUnprotectArrayFromFreeAll(mxArgsOut[0]);
            mxDestroyArray(mxArgsOut[0]);
        }
        mxFree(mxArgsOut);
        mxArgsOut = nullptr;
    }
    if (mxArgsIn != nullptr) {
        mxUnprotectPointerFromFreeAll(mxArgsIn);
        for (size_t i = 0; i < argIn.size(); i++) {
            mxUnprotectArrayFromFreeAll(mxArgsIn[i]);
            mxDestroyArray(mxArgsIn[i]);
        }
        mxFree(mxArgsIn);
        mxArgsIn = nullptr;
    }
    return 0;
}
//=============================================================================
