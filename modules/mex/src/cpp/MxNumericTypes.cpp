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
#include "MxNumericTypes.h"
//=============================================================================
mxArray*
mxCreateNumericArraySeparatedComplex(
    mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(ndim, dims, sizeFromClass(classid), classid);
    }
    return mxAllocateSeparatedComplexArray(ndim, dims, sizeFromClass(classid), classid);
}
//=============================================================================
mxArray*
mxCreateNumericArrayInterleavedComplex(
    mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(ndim, dims, sizeFromClass(classid), classid);
    }
    return mxAllocateInterleavedComplexArray(ndim, dims, sizeFromClass(classid), classid);
}
//=============================================================================
mxArray*
mxCreateDoubleMatrixSeparatedComplex(mwSize m, mwSize n, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
    }
    return mxAllocateSeparatedComplexArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
}
//=============================================================================
mxArray*
mxCreateDoubleMatrixInterleavedComplex(mwSize m, mwSize n, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
    }
    return mxAllocateInterleavedComplexArray(2, dims, sizeof(mxDouble), mxDOUBLE_CLASS);
}
//=============================================================================
mxArray*
mxCreateDoubleScalar(double value)
{
    mxArray* ret = mxCreateDoubleMatrix(1, 1, mxREAL);
    if (ret != nullptr) {
        ((double*)ret->realdata)[0] = value;
    }
    return ret;
}
//=============================================================================
mxArray*
mxCreateNumericMatrixSeparatedComplex(
    mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateNumericArraySeparatedComplex(2, dims, classid, ComplexFlag);
}
//=============================================================================
mxArray*
mxCreateNumericMatrixInterleavedComplex(
    mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    return mxCreateNumericArrayInterleavedComplex(2, dims, classid, ComplexFlag);
}
//=============================================================================
mxArray*
mxCreateUninitNumericMatrixSeparatedComplex(
    size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateSeparatedComplexArray(2, dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
mxArray*
mxCreateUninitNumericMatrixInterleavedComplex(
    size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag)
{
    mwSize dims[2];
    dims[0] = m;
    dims[1] = n;
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(2, dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateInterleavedComplexArray(2, dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
mxArray*
mxCreateUninitNumericArraySeparatedComplex(
    size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(
            (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateSeparatedComplexArray(
        (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
mxArray*
mxCreateUninitNumericArrayInterleavedComplex(
    size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag)
{
    if (ComplexFlag == mxREAL) {
        return mxAllocateRealArray(
            (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
    }
    return mxAllocateSeparatedComplexArray(
        (mwSize)ndim, (const mwSize*)dims, sizeFromClass(classid), classid, false);
}
//=============================================================================
