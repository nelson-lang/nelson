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
#include "matrix.h"
#include "MxHelpers.hpp"
//=============================================================================
mxArray*
mxCreateNumericArray(mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(ndim, dims, sizeFromClass(classid), classid);
    }
    return mxAllocateComplexArray(ndim, dims, sizeFromClass(classid), classid);
}
//=============================================================================
mxArray*
mxCreateDoubleMatrix(mwSize m, mwSize n, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
    }
    return mxAllocateComplexArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
}
//=============================================================================
mxArray*
mxCreateDoubleScalar(double value)
{
    mxArray* ret = mxCreateDoubleMatrix(1, 1, mxREAL);
    if (ret) {
        ((double*)ret->realdata)[0] = value;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateNumericMatrix(mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateNumericArray(2, dims, classid, ComplexFlag);
}
//=============================================================================
mxArray*
mxCreateUninitNumericMatrix(size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateComplexArray(2, dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
mxArray*
mxCreateUninitNumericArray(size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(
            (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateComplexArray(
        (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
