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
#include "MxCallBuiltin.hpp"
#include "ArrayOf.hpp"
#include "MxArrayOf.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "mex.h"
//=============================================================================
using MexFuncPtr = void (*)(int, mxArray**, int, const mxArray**);
//=============================================================================
int
mxCallBuiltin(
    void* fptr, const Nelson::ArrayOfVector& argIn, int nargout, Nelson::ArrayOfVector& argOut)
{
    mxArray** mxArgsIn = nullptr;
    mxArray** mxArgsOut = nullptr;

    try {
        mxArgsIn = new mxArray*[argIn.size()];
    } catch (const std::bad_alloc&) {
        Nelson::Error(ERROR_MEMORY_ALLOCATION);
    }
    int nlhs = (int)argIn.size();
    int lhsCount = nargout;
    lhsCount = (lhsCount < 1) ? 1 : lhsCount;
    try {
        mxArgsOut = new mxArray*[lhsCount];
        for (size_t i = 0; i < lhsCount; ++i) {
            mxArgsOut[i] = nullptr;
        }
    } catch (const std::bad_alloc&) {
        for (size_t i = 0; i < argIn.size(); i++) {
            mxDestroyArray(mxArgsIn[i]);
        }
        delete[] mxArgsIn;
        mxArgsIn = nullptr;
        Nelson::Error(ERROR_MEMORY_ALLOCATION);
    }

    for (size_t i = 0; i < argIn.size(); ++i) {
        mxArgsIn[i] = Nelson::ArrayOfToMxArray(argIn[i]);
    }

    MexFuncPtr builtinPtr = (MexFuncPtr)fptr;

    try {
        builtinPtr(nargout, mxArgsOut, nlhs, (const mxArray**)mxArgsIn);
    } catch (const std::runtime_error& e) {
        for (size_t i = 0; i < argIn.size(); i++) {
            mxDestroyArray(mxArgsIn[i]);
        }
        delete[] mxArgsIn;
        mxArgsIn = nullptr;

        for (int i = 0; i < lhsCount; i++) {
            mxDestroyArray(mxArgsOut[i]);
        }
        delete[] mxArgsOut;
        mxArgsOut = nullptr;
        Nelson::Error(e.what());
    } catch (Nelson::Exception& e) {
        for (size_t i = 0; i < argIn.size(); i++) {
            mxDestroyArray(mxArgsIn[i]);
        }
        delete[] mxArgsIn;
        mxArgsIn = nullptr;

        for (int i = 0; i < lhsCount; i++) {
            mxDestroyArray(mxArgsOut[i]);
        }
        delete[] mxArgsOut;
        mxArgsOut = nullptr;
        throw e;
    }
    for (int i = 0; i < lhsCount; i++) {
        argOut.push_back(Nelson::MxArrayToArrayOf(mxArgsOut[i]));
        mxDestroyArray(mxArgsOut[i]);
    }
    delete[] mxArgsOut;
    mxArgsOut = nullptr;

    for (size_t i = 0; i < argIn.size(); i++) {
        mxDestroyArray(mxArgsIn[i]);
    }
    delete[] mxArgsIn;
    mxArgsIn = nullptr;
    return 0;
}
//=============================================================================
