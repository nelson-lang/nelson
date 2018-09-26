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

#pragma once

#include <complex>
#include <string>
#include <vector>
#include <cstddef>
#include <stdint.h>
#include "nlsConfig.h"

namespace Nelson {

#if (defined(_LP64) || defined(_WIN64))
#define NLS_INDEX_TYPE_64
#endif

#ifdef NLS_INDEX_TYPE_64 // 64 bits
typedef size_t sizeType;
typedef size_t indexType;
typedef ptrdiff_t signedIndexType;

#define SIZE_TYPE_MAX 281474976710655UL
#define INDEX_TYPE_MAX 281474976710655UL
#define SIGNED_INDEX_TYPE_MAX 281474976710655UL
#define SIGNED_INDEX_TYPE_MIN -281474976710655UL
#else // 32 bits
typedef int sizeType;
typedef int signedIndexType;
typedef int indexType;

#define SIZE_TYPE_MAX 2147483647UL
#define INDEX_TYPE_MAX 2147483647UL
#define SIGNED_INDEX_TYPE_MAX 2147483647UL
#define SIGNED_INDEX_TYPE_MIN -2147483647UL
#endif

#define SIZE_TYPE_MIN 0UL
#define UNSIGNED_INDEX_TYPE_MIN 0UL

typedef uint8_t logical;
typedef int8_t int8;
typedef uint8_t uint8;
typedef int16_t int16;
typedef uint16_t uint16;
typedef int32_t int32;
typedef uint32_t uint32;
typedef int64_t int64;

typedef uint64_t uint64;
typedef wchar_t charType;
typedef float single;
typedef const indexType* constIndexPtr;
typedef size_t function_handle;
typedef long nelson_handle;

#define doublecomplex std::complex<double>
#define singlecomplex std::complex<single>

#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

typedef enum
{
    NLS_NOT_TYPED = -1,
    NLS_HANDLE = 0,
    NLS_CELL_ARRAY,
    NLS_STRUCT_ARRAY,
    NLS_STRING_ARRAY,
    NLS_LOGICAL,
    NLS_UINT8,
    NLS_INT8,
    NLS_UINT16,
    NLS_INT16,
    NLS_UINT32,
    NLS_INT32,
    NLS_UINT64,
    NLS_INT64,
    NLS_SINGLE,
    NLS_DOUBLE,
    NLS_SCOMPLEX,
    NLS_DCOMPLEX,
    NLS_CHAR,
} Class;
typedef std::vector<std::string> stringVector;
typedef std::vector<std::wstring> wstringVector;

#define NLS_SPARSE_STR "sparse"
#define NLS_CELL_ARRAY_STR "cell"
#define NLS_STRUCT_ARRAY_STR "struct"
#define NLS_STRING_ARRAY_STR "string"
#define NLS_LOGICAL_STR "logical"
#define NLS_UINT8_STR "uint8"
#define NLS_INT8_STR "int8"
#define NLS_UINT16_STR "uint16"
#define NLS_INT16_STR "int16"
#define NLS_UINT32_STR "uint32"
#define NLS_INT32_STR "int32"
#define NLS_UINT64_STR "uint64"
#define NLS_INT64_STR "int64"
#define NLS_SINGLE_STR "single"
#define NLS_DOUBLE_STR "double"
#define NLS_SCOMPLEX_STR "single"
#define NLS_DCOMPLEX_STR "double"
#define NLS_CHAR_STR "char"
#define NLS_FUNCTION_HANDLE_STR "function_handle"
#define NLS_HANDLE_STR "handle"
#define NLS_GENERIC_STR "generic"
#define NLS_INTEGER_STR "integer"

}
