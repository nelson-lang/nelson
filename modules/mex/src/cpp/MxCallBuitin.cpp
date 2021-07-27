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
#if _MSC_VER
#pragma warning(disable : 4297)
#endif
//=============================================================================
#include <stdexcept>
#include "MxCallBuiltin.hpp"
#include "ArrayOf.hpp"
#include "MxArrayOf.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "mex.h"
#include "MxHelpers.hpp"
//=============================================================================
using MexFuncPtr = void (*)(int, mxArray**, int, const mxArray**);
//=============================================================================
int
mxCallBuiltin(void* fptr, const Nelson::ArrayOfVector& argIn, int nargout,
    Nelson::ArrayOfVector& argOut, bool interleavedComplex)
{
    mxArray** mxArgsIn = nullptr;
    mxArray** mxArgsOut = nullptr;

    try {
        if (!argIn.empty()) {
            mxArgsIn = (mxArray**)mxMalloc(sizeof(mxArray*) * argIn.size());
        }
    } catch (const std::bad_alloc&) {
        Nelson::Error(ERROR_MEMORY_ALLOCATION);
    }
    int nlhs = (int)argIn.size();
    int lhsCount = (nargout < 1) ? 1 : nargout;
    try {
        mxArgsOut = (mxArray**)mxMalloc(mwSize(sizeof(mxArray*) * lhsCount));
        for (size_t i = 0; i < (size_t)lhsCount; ++i) {
            mxArgsOut[i] = mxNewArray();
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
        Nelson::Error(ERROR_MEMORY_ALLOCATION);
    }

    if (mxArgsIn != nullptr) {
        for (size_t i = 0; i < argIn.size(); ++i) {
            mxArgsIn[i] = Nelson::ArrayOfToMxArray(argIn[i], interleavedComplex);
        }
    }

    auto builtinPtr = (MexFuncPtr)fptr;

    try {
        builtinPtr(nargout, mxArgsOut, nlhs, (const mxArray**)mxArgsIn);
    } catch (const std::runtime_error& e) {
        if (mxArgsIn != nullptr) {
            for (size_t i = 0; i < argIn.size(); i++) {
                mxDestroyArray(mxArgsIn[i]);
            }
            mxFree(mxArgsIn);
            mxArgsIn = nullptr;
        }
        if (mxArgsOut != nullptr) {
            for (int i = 0; i < lhsCount; i++) {
                mxDestroyArray(mxArgsOut[i]);
            }
            mxFree(mxArgsOut);
            mxArgsOut = nullptr;
        }
        Nelson::Error(e.what());
    } catch (Nelson::Exception& e) {
        if (mxArgsIn != nullptr) {
            for (size_t i = 0; i < argIn.size(); i++) {
                mxDestroyArray(mxArgsIn[i]);
            }
            mxFree(mxArgsIn);
            mxArgsIn = nullptr;
        }
        if (mxArgsOut != nullptr) {
            const int protectLhsCount = lhsCount;
            for (int i = 0; i < protectLhsCount; i++) {
                mxDestroyArray(mxArgsOut[i]);
            }
            mxFree(mxArgsOut);
            mxArgsOut = nullptr;
        }
        throw e;
    }
    if (mxArgsOut != nullptr) {
        bool noOutput = (lhsCount == 1) && (mxArgsOut[0]->classID == mxUNKNOWN_CLASS)
            && (mxArgsOut[0]->number_of_dims == 0);
        if (!noOutput) {
            for (int i = 0; i < lhsCount; i++) {
                argOut.push_back(Nelson::MxArrayToArrayOf(mxArgsOut[i]));
                mxDestroyArray(mxArgsOut[i]);
            }
        } else {
            mxDestroyArray(mxArgsOut[0]);
        }
        mxFree(mxArgsOut);
        mxArgsOut = nullptr;
    }
    if (mxArgsIn != nullptr) {
        for (size_t i = 0; i < argIn.size(); i++) {
            mxDestroyArray(mxArgsIn[i]);
        }
        mxFree(mxArgsIn);
        mxArgsIn = nullptr;
    }
    return 0;
}
//=============================================================================
