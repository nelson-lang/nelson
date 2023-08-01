//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <complex>
#include <string>
#include <vector>
#include <cstddef>
#include <cstdint>
//=============================================================================
namespace Nelson {

#define PTR_TO_NELSON_HANDLE(x) (uint64_t)(uintptr_t)(x)
#define NELSON_HANDLE_TO_PTR(x) (uintptr_t)(uint64_t)(x)

#if (defined(_LP64) || defined(_WIN64))
#define NLS_INDEX_TYPE_64
#endif

#ifdef NLS_INDEX_TYPE_64 // 64 bits
using sizeType = size_t;
using indexType = size_t;
using signedIndexType = ptrdiff_t;
using ompIndexType = long long;

#define SIZE_TYPE_MAX 281474976710655UL
#define INDEX_TYPE_MAX 281474976710655UL
#define SIGNED_INDEX_TYPE_MAX 281474976710655UL
#define SIGNED_INDEX_TYPE_MIN -281474976710655UL
#else // 32 bits
using sizeType = int;
using signedIndexType = int;
using indexType = int;
using ompIndexType = int;

#define SIZE_TYPE_MAX 2147483647UL
#define INDEX_TYPE_MAX 2147483647UL
#define SIGNED_INDEX_TYPE_MAX 2147483647UL
#define SIGNED_INDEX_TYPE_MIN -2147483647UL
#endif

#define SIZE_TYPE_MIN 0UL
#define UNSIGNED_INDEX_TYPE_MIN 0UL

using logical = uint8_t;
using int8 = int8_t;
using uint8 = uint8_t;
using int16 = int16_t;
using uint16 = uint16_t;
using int32 = int32_t;
using uint32 = uint32_t;
using int64 = int64_t;

using uint64 = uint64_t;
using charType = wchar_t;
using single = float;
using constIndexPtr = const indexType*;

using nelson_handle = uint64_t;
using go_handle = int64_t;

struct function_handle
{
    std::string name;
    nelson_handle* anonymousHandle;
};

#define doublecomplex std::complex<double>
#define singlecomplex std::complex<single>

#ifndef false
#define false 0
#endif
#ifndef true
#define true 1
#endif

enum NelsonType
{
    NLS_DOUBLE = 0,
    NLS_SINGLE,
    NLS_DCOMPLEX,
    NLS_SCOMPLEX,
    NLS_INT8,
    NLS_INT16,
    NLS_INT32,
    NLS_INT64,
    NLS_UINT8,
    NLS_UINT16,
    NLS_UINT32,
    NLS_UINT64,
    NLS_LOGICAL,
    NLS_CHAR,
    NLS_STRUCT_ARRAY,
    NLS_CLASS_ARRAY,
    NLS_CELL_ARRAY,
    NLS_STRING_ARRAY,
    NLS_HANDLE,
    NLS_GO_HANDLE,
    /* NLS_FUNCTION_HANDLE, */
    NLS_UNKNOWN,
};

using stringVector = std::vector<std::string>;
using wstringVector = std::vector<std::wstring>;

#define NLS_SPARSE_STR "sparse"
#define NLS_CELL_ARRAY_STR "cell"
#define NLS_STRUCT_ARRAY_STR "struct"
#define NLS_CLASS_ARRAY_STR "class"
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
#define NLS_GO_HANDLE_STR "graphics_object"
#define NLS_HANDLE_STR "handle"
#define NLS_GENERIC_STR "generic"
#define NLS_INTEGER_STR "integer"
#define NLS_UNKNOWN_STR "unknown"
//=============================================================================
} // namespace Nelson
//=============================================================================
