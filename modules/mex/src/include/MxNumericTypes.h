//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsMex_exports.h"
#include "MxTypes.h"
#include "MxAsserts.h"
#include "MxAttributes.h"
#include "MxCell.h"
#include "MxCharacter.h"
#include "MxConstants.h"
#include "MxLogical.h"
#include "MxMemory.h"
#include "MxNonComplexFloat.h"
#include "MxNumericTypes.h"
#include "MxStruct.h"
#include "MxObject.h"
#include "MxCall.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
    NLSMEX_IMPEXP mxArray*
    mxCreateNumericArraySeparatedComplex(
        mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP mxArray*
    mxCreateNumericArrayInterleavedComplex(
        mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleMatrixSeparatedComplex(mwSize m, mwSize n, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleMatrixInterleavedComplex(mwSize m, mwSize n, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleScalar(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericMatrixSeparatedComplex(
        mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericMatrixInterleavedComplex(
        mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericMatrixSeparatedComplex(
        size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericMatrixInterleavedComplex(
        size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericArraySeparatedComplex(
        size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericArrayInterleavedComplex(
        size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
#ifdef MX_HAS_INTERLEAVED_COMPLEX
#define mxCreateDoubleMatrix mxCreateDoubleMatrixInterleavedComplex
#define mxCreateNumericMatrix mxCreateNumericMatrixInterleavedComplex
#define mxCreateNumericArray mxCreateNumericArrayInterleavedComplex
#define mxCreateUninitNumericArray mxCreateUninitNumericArrayInterleavedComplex
#else
#define mxCreateDoubleMatrix mxCreateDoubleMatrixSeparatedComplex
#define mxCreateNumericMatrix mxCreateNumericMatrixSeparatedComplex
#define mxCreateNumericArray mxCreateNumericArraySeparatedComplex
#define mxCreateUninitNumericArray mxCreateUninitNumericArraySeparatedComplex
#endif
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
