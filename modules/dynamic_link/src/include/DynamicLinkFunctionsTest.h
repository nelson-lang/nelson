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
#include "nlsDynamic_link_exports.h"
#include <cstdint>
//=============================================================================
extern "C"
{
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestReturnTypeVoid(void);
    NLSDYNAMIC_LINK_IMPEXP unsigned char
    dynlibTestReturnTypeScalarUInt8(void);
    NLSDYNAMIC_LINK_IMPEXP int8_t
    dynlibTestReturnTypeScalarInt8(void);
    NLSDYNAMIC_LINK_IMPEXP uint16_t
    dynlibTestReturnTypeScalarUInt16(void);
    NLSDYNAMIC_LINK_IMPEXP int16_t
    dynlibTestReturnTypeScalarInt16(void);
    NLSDYNAMIC_LINK_IMPEXP uint32_t
    dynlibTestReturnTypeScalarUInt32(void);
    NLSDYNAMIC_LINK_IMPEXP int
    dynlibTestReturnTypeScalarInt32(void);
    NLSDYNAMIC_LINK_IMPEXP uint64_t
    dynlibTestReturnTypeScalarUInt64(void);
    NLSDYNAMIC_LINK_IMPEXP int64_t
    dynlibTestReturnTypeScalarInt64(void);
    NLSDYNAMIC_LINK_IMPEXP char*
    dynlibTestReturnTypeCString(void);
    NLSDYNAMIC_LINK_IMPEXP wchar_t*
    dynlibTestReturnTypeWString(void);
    NLSDYNAMIC_LINK_IMPEXP double
    dynlibTestReturnTypeScalarDouble(void);
    NLSDYNAMIC_LINK_IMPEXP float
    dynlibTestReturnTypeScalarSingle(void);
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint8_t
    dynlibTestInputUInt8(uint8_t x);
    NLSDYNAMIC_LINK_IMPEXP int8_t
    dynlibTestInputInt8(int8_t x);
    NLSDYNAMIC_LINK_IMPEXP uint16_t
    dynlibTestInputUInt16(uint16_t x);
    NLSDYNAMIC_LINK_IMPEXP int16_t
    dynlibTestInputInt16(int16_t x);
    NLSDYNAMIC_LINK_IMPEXP uint32_t
    dynlibTestInputUInt32(uint32_t x);
    NLSDYNAMIC_LINK_IMPEXP int32_t
    dynlibTestInputInt32(int32_t x);
    NLSDYNAMIC_LINK_IMPEXP uint64_t
    dynlibTestInputUInt64(uint64_t x);
    NLSDYNAMIC_LINK_IMPEXP int64_t
    dynlibTestInputInt64(int64_t x);
    NLSDYNAMIC_LINK_IMPEXP float
    dynlibTestInputFloat(float x);
    NLSDYNAMIC_LINK_IMPEXP double
    dynlibTestInputDouble(double x);
    NLSDYNAMIC_LINK_IMPEXP char*
    dynlibTestInputCString(char* x);
    NLSDYNAMIC_LINK_IMPEXP wchar_t*
    dynlibTestInputWString(wchar_t* x);
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt8Ptr(uint8_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt8Ptr(int8_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt16Ptr(uint16_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt16Ptr(int16_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt32Ptr(uint32_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt32Ptr(int32_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt64Ptr(uint64_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt64Ptr(int64_t* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputFloatPtr(float* x, int size);
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputDoublePtr(double* x, int size);
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int
    dynlibTestMultiplyDoubleArrayWithReturn(double* x, int size);
    NLSDYNAMIC_LINK_IMPEXP double
    sumDoubleRef(double x, const double* y, double z);
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP double*
    multiplicationDoubleByReference(double* x);
    //=============================================================================
}
//=============================================================================
