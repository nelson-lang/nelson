//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include "DynamicLinkFunctionsTest.h"
#include <ctype.h> // toupper
#include <string.h> // strlen
#include <wchar.h> // wcslen
#include <wctype.h> // towupper
//=============================================================================
extern "C"
{
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestReturnTypeVoid(void)
    {}
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint8_t
    dynlibTestReturnTypeScalarUInt8(void)
    {
        return uint8_t(8);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int8_t
    dynlibTestReturnTypeScalarInt8(void)
    {
        return int8_t(9);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint16_t
    dynlibTestReturnTypeScalarUInt16(void)
    {
        return uint16_t(16);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int16_t
    dynlibTestReturnTypeScalarInt16(void)
    {
        return int16_t(17);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint32_t
    dynlibTestReturnTypeScalarUInt32(void)
    {
        return uint32_t(32);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int
    dynlibTestReturnTypeScalarInt32(void)
    {
        return (int)(33);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint64_t
    dynlibTestReturnTypeScalarUInt64(void)
    {
        return (uint64_t)(64);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int64_t
    dynlibTestReturnTypeScalarInt64(void)
    {
        return (int64_t)(65);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP double
    dynlibTestReturnTypeScalarDouble(void)
    {
        return 3.33000;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP float
    dynlibTestReturnTypeScalarSingle(void)
    {
        return (float)(6.66000);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP char*
    dynlibTestReturnTypeCString(void)
    {
#define UTF8_STR_TO_SEND "hello utf-8"
        char* str = new char[strlen(UTF8_STR_TO_SEND) + 1];
        strcpy(str, UTF8_STR_TO_SEND);
        return str;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP wchar_t*
    dynlibTestReturnTypeWString(void)
    {
#define UNICODE_STR_TO_SEND L"hello unicode"
        wchar_t* str = new wchar_t[wcslen(UNICODE_STR_TO_SEND) + 1];
        wcscpy(str, L"hello unicode");
        return str;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint8_t
    dynlibTestInputUInt8(uint8_t x)
    {
        return x + 1;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int8_t
    dynlibTestInputInt8(int8_t x)
    {
        return x + 2;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint16_t
    dynlibTestInputUInt16(uint16_t x)
    {
        return x + 3;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int16_t
    dynlibTestInputInt16(int16_t x)
    {
        return x + 4;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint32_t
    dynlibTestInputUInt32(uint32_t x)
    {
        return x + 5;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int32_t
    dynlibTestInputInt32(int32_t x)
    {
        return x + 6;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP uint64_t
    dynlibTestInputUInt64(uint64_t x)
    {
        return x + 7;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int64_t
    dynlibTestInputInt64(int64_t x)
    {
        return x + 8;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP float
    dynlibTestInputFloat(float x)
    {
        return x + 9;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP double
    dynlibTestInputDouble(double x)
    {
        return x + 10;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP char*
    dynlibTestInputCString(char* x)
    {
        size_t len = strlen(x);
        char* res = new char[len + 1];
        memset(res, 0, len + 1);
        for (size_t k = 0; k < len; k++) {
            res[k] = toupper(x[k]);
        }
        return res;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP wchar_t*
    dynlibTestInputWString(wchar_t* x)
    {
        size_t len = wcslen(x);
        wchar_t* res = new wchar_t[len + 1];
        memset(res, 0, (len + 1) * sizeof(wchar_t));
        for (size_t k = 0; k < len; k++) {
            res[k] = towupper(x[k]);
        }
        return res;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt8Ptr(uint8_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= uint8_t(2);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt8Ptr(int8_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= int8_t(3);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt16Ptr(uint16_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= uint16_t(4);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt16Ptr(int16_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= int16_t(5);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt32Ptr(uint32_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= uint32_t(6);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt32Ptr(int32_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= int32_t(7);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputUInt64Ptr(uint64_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= uint64_t(8);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputInt64Ptr(int64_t* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= int64_t(9);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputFloatPtr(float* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= float(10);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP void
    dynlibTestInputDoublePtr(double* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= double(11);
        }
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP int
    dynlibTestMultiplyDoubleArrayWithReturn(double* x, int size)
    {
        for (int l = 0; l < size; l++) {
            *x++ *= 6;
        }
        return 44;
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP double
    sumDoubleRef(double x, double* y, double z)
    {
        return (x + *y + z);
    }
    //=============================================================================
    NLSDYNAMIC_LINK_IMPEXP double*
    multiplicationDoubleByReference(double* x)
    {
        *x *= 2;
        return x;
    }
    //=============================================================================
}
//=============================================================================
