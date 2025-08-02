//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
