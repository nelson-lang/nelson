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
#pragma once
//=============================================================================
#include <stdint.h>
#include <stddef.h>
#include "nlsMex_exports.h"
//=============================================================================
#ifdef __cplusplus
extern "C"
{
#endif
    //=============================================================================
#if defined(_LP64) || defined(_WIN64)
    typedef size_t mwSize;
    typedef size_t mwIndex;
    typedef ptrdiff_t mwSignedIndex;
#else
typedef int mwSize;
typedef int mwIndex;
typedef int mwSignedIndex;
#endif
    //=============================================================================
    typedef enum
    {
        mxUNKNOWN_CLASS = 0,
        mxCELL_CLASS,
        mxSTRUCT_CLASS,
        mxLOGICAL_CLASS,
        mxCHAR_CLASS,
        mxVOID_CLASS,
        mxDOUBLE_CLASS,
        mxSINGLE_CLASS,
        mxINT8_CLASS,
        mxUINT8_CLASS,
        mxINT16_CLASS,
        mxUINT16_CLASS,
        mxINT32_CLASS,
        mxUINT32_CLASS,
        mxINT64_CLASS,
        mxUINT64_CLASS,
        mxFUNCTION_CLASS,
        mxOPAQUE_CLASS,
        mxOBJECT_CLASS,
#if defined(_LP64) || defined(_WIN64)
        mxINDEX_CLASS = mxUINT64_CLASS,
#else
    mxINDEX_CLASS = mxUINT32_CLASS,
#endif
    } mxClassID;
    //=============================================================================
#if !defined(__cplusplus) && !defined(bool)
    typedef int8_t bool;
#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif
#endif
    //=============================================================================
    typedef bool mxLogical;
    typedef unsigned short mxChar;
    //=============================================================================
    typedef enum
    {
        mxREAL = 0,
        mxCOMPLEX
    } mxComplexity;
    //=============================================================================
    typedef double mxDouble;
    typedef float mxSingle;
    typedef int8_t mxInt8;
    typedef uint8_t mxUint8;
    typedef int16_t mxInt16;
    typedef uint16_t mxUint16;
    typedef int32_t mxInt32;
    typedef uint32_t mxUint32;
    typedef int64_t mxInt64;
    typedef uint64_t mxUint64;
    //=============================================================================
    typedef struct
    {
        mxDouble real, imag;
    } mxComplexDouble;
    typedef struct
    {
        mxSingle real, imag;
    } mxComplexSingle;
    typedef struct
    {
        mxInt8 real, imag;
    } mxComplexInt8;
    typedef struct
    {
        mxUint8 real, imag;
    } mxComplexUint8;
    typedef struct
    {
        mxInt16 real, imag;
    } mxComplexInt16;
    typedef struct
    {
        mxUint16 real, imag;
    } mxComplexUint16;
    typedef struct
    {
        mxInt32 real, imag;
    } mxComplexInt32;
    typedef struct
    {
        mxUint32 real, imag;
    } mxComplexUint32;
    typedef struct
    {
        mxInt64 real, imag;
    } mxComplexInt64;
    typedef struct
    {
        mxUint64 real, imag;
    } mxComplexUint64;
    //=============================================================================
    struct mxArray_tag
    {
        mxClassID classID;
        mwSize* dims;
        mwSize number_of_dims;
        bool issparse;
        int iscomplex;
        void* realdata;
        void* imagdata;
        uint64_t* ptr;
    };
    //=============================================================================
    typedef struct mxArray_tag mxArray;
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxMalloc(mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxCalloc(mwSize n, mwSize size);
    //=============================================================================
    NLSMEX_IMPEXP
    void*
    mxRealloc(void* ptr, mwSize size);
    //=============================================================================
    NLSMEX_IMPEXP void
    mxFree(void* ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxDestroyArray(mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxDuplicateArray(const mxArray* in);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetNumberOfElements(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericArray(
        mwSize ndim, const mwSize* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    char*
    mxArrayToString(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    char*
    mxArrayToUTF8String(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxGetString(const mxArray* pm, char* str, mwSize strlen);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateString(const char* str);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateCharMatrixFromStrings(mwSize m, const char** str);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateCharArray(mwSize ndim, const mwSize* dims);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleMatrix(mwSize m, mwSize n, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    double
    mxGetNaN(void);
    //=============================================================================
    NLSMEX_IMPEXP
    double
    mxGetInf(void);
    //=============================================================================
    NLSMEX_IMPEXP
    double
    mxGetEps(void);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsNumeric(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsComplex(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mwSize
    mxGetNumberOfDimensions(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetElementSize(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    const mwSize*
    mxGetDimensions(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetDimensions(mxArray* pm, const mwSize* dims, mwSize ndim);
    //=============================================================================
    NLSMEX_IMPEXP
    mwIndex
    mxCalcSingleSubscript(const mxArray* pm, mwSize nsubs, mwIndex* subs);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetM(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    size_t
    mxGetN(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetM(mxArray* pm, mwSize m);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetN(mxArray* pm, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsEmpty(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsFromGlobalWS(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsInf(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsNaN(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsFinite(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mexPrintAssertion(const char* test, const char* fname, int linenum, const char* message);
    //=============================================================================
#define mxAssert(test, message)                                                                    \
    ((test) ? (void)0 : mexPrintAssertion(#test, __FILE__, __LINE__, message))
    //=============================================================================
#define mxAssertS(test, message)                                                                   \
    ((test) ? (void)0 : mexPrintAssertion("", __FILE__, __LINE__, message))
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateDoubleScalar(double value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateNumericMatrix(mwSize m, mwSize n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericMatrix(size_t m, size_t n, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateUninitNumericArray(
        size_t ndim, size_t* dims, mxClassID classid, mxComplexity ComplexFlag);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsScalar(const mxArray* array_ptr);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsClass(const mxArray* pm, const char* classname);
    //=============================================================================
    NLSMEX_IMPEXP
    double
    mxGetScalar(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsDouble(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetDoubles(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetDoubles(mxArray* pa, mxDouble* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsSingle(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxSingle*
    mxGetSingles(const mxArray* pa);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxSetSingles(mxArray* pa, mxSingle* dt);
    //=============================================================================
    NLSMEX_IMPEXP
    mxDouble*
    mxGetPr(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetPr(mxArray* pm, double* pr);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalScalar(mxLogical value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalMatrix(mwSize m, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateLogicalArray(mwSize ndim, const mwSize* dims);
    //=============================================================================

    NLSMEX_IMPEXP
    mxArray*
    mxCreateCellArray(mwSize ndim, const mwSize* dims);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateCellMatrix(mwSize m, mwSize n);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsCell(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxGetCell(const mxArray* pm, mwIndex index);
    //=============================================================================
    NLSMEX_IMPEXP
    void
    mxSetCell(mxArray* pm, mwIndex index, mxArray* value);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateStructArray(mwSize ndim, const mwSize* dims, int nfields, const char** fieldnames);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxCreateStructMatrix(mwSize m, mwSize n, int nfields, const char** fieldnames);
    //=============================================================================
    NLSMEX_IMPEXP
    bool
    mxIsStruct(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    int
    mxGetNumberOfFields(const mxArray* pm);
    //=============================================================================
    NLSMEX_IMPEXP
    mxArray*
    mxGetFieldByNumber(const mxArray* pm, mwIndex index, int fieldnumber);
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
