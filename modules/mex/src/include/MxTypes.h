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
#include <stdint.h>
#include <stddef.h>
#if !defined(__cplusplus)
#include <stdbool.h>
#endif
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
#if !defined(__cplusplus)
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
        mwSize nzmax;
        mwSize nJc;
        mwSize nIr;
        bool iscomplex;
        bool interleavedcomplex;
        bool persistentmemory;
        bool issparse;
        void* realdata;
        void* imagdata;
        mwIndex* Jc;
        mwIndex* Ir;
        uint64_t* ptr;
    };
    //=============================================================================
    typedef struct mxArray_tag mxArray;
    //=============================================================================
#ifdef __cplusplus
}
#endif
//=============================================================================
