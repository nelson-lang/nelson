//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaLibraryWrapper.hpp"
#include "DynamicLibrary.hpp"
#include "characters_encoding.hpp"
#include <unordered_map>
#ifdef _MSC_VER
#pragma warning(disable : 4068)
#pragma warning(disable : 4200)
#pragma warning(disable : 4359)
#endif
//=============================================================================
static bool juliaLibraryLoaded = false;
static Nelson::library_handle julia_handle = nullptr;
static std::unordered_map<std::string, void*> juliaSymbols;
//=============================================================================
#define LOAD_JULIA_SYMBOL(symbol_name)                                                             \
    juliaSymbols[#symbol_name]                                                                     \
        = reinterpret_cast<void*>(Nelson::get_function(julia_handle, #symbol_name));               \
    if (!juliaSymbols[#symbol_name]) {                                                             \
        printf("Julia symbol not found: %s\n", #symbol_name);                                      \
        return false;                                                                              \
    }
//=============================================================================
static bool
loadJuliaSymbols();
//=============================================================================
using PROC_jl_init = void (*)();
using PROC_jl_eval_string = jl_value_t* (*)(const char*);
using PROC_jl_exception_occurred = jl_value_t* (*)(void);
using PROC_jl_typeof_str = const char* (*)(jl_value_t* v);
using PROC_jl_string_ptr = const char* (*)(jl_value_t* s);
using PROC_jl_exception_clear = void (*)();
using PROC_jl_get_global = jl_value_t* (*)(jl_module_t*, jl_sym_t*);
using PROC_jl_set_global = void (*)(jl_module_t* m, jl_sym_t* var, jl_value_t* val);
using PROC_jl_unbox_bool = int8_t (*)(jl_value_t* v);
using PROC_jl_unbox_float32 = float (*)(jl_value_t*);
using PROC_jl_unbox_float64 = double (*)(jl_value_t*);
using PROC_jl_unbox_int8 = int8_t (*)(jl_value_t*);
using PROC_jl_unbox_uint8 = uint8_t (*)(jl_value_t*);
using PROC_jl_unbox_int16 = int16_t (*)(jl_value_t*);
using PROC_jl_unbox_uint16 = uint16_t (*)(jl_value_t*);
using PROC_jl_unbox_int32 = int32_t (*)(jl_value_t*);
using PROC_jl_unbox_uint32 = uint32_t (*)(jl_value_t*);
using PROC_jl_unbox_int64 = int64_t (*)(jl_value_t*);
using PROC_jl_unbox_uint64 = uint64_t (*)(jl_value_t*);
using PROC_jl_symbol = jl_sym_t* (*)(const char* str);
using PROC_jl_array_eltype = void* (*)(jl_value_t* a);
using PROC_jl_get_field = jl_value_t* (*)(jl_value_t* o, const char* fld);
using PROC_jl_field_index = int (*)(jl_datatype_t* t, jl_sym_t* fld, int err);
using PROC_jl_call = jl_value_t* (*)(jl_function_t* f, jl_value_t** args, uint32_t nargs);
using PROC_jl_call0 = jl_value_t* (*)(jl_function_t*);
using PROC_jl_call1 = jl_value_t* (*)(jl_function_t*, jl_value_t*);
using PROC_jl_call2 = jl_value_t* (*)(jl_function_t* f, jl_value_t* a, jl_value_t* b);
using PROC_jl_call3 = jl_value_t* (*)(jl_function_t*, jl_value_t*, jl_value_t*, jl_value_t*);
using PROC_jl_box_bool = jl_value_t* (*)(int8_t x);
using PROC_jl_box_int8 = jl_value_t* (*)(int8_t x);
using PROC_jl_box_uint8 = jl_value_t* (*)(uint8_t x);
using PROC_jl_box_int16 = jl_value_t* (*)(int16_t x);
using PROC_jl_box_uint16 = jl_value_t* (*)(uint16_t x);
using PROC_jl_box_int32 = jl_value_t* (*)(int32_t x);
using PROC_jl_box_uint32 = jl_value_t* (*)(uint32_t x);
using PROC_jl_box_char = jl_value_t* (*)(uint32_t x);
using PROC_jl_box_int64 = jl_value_t* (*)(int64_t x);
using PROC_jl_box_uint64 = jl_value_t* (*)(uint64_t x);
using PROC_jl_box_float32 = jl_value_t* (*)(float x);
using PROC_jl_box_float64 = jl_value_t* (*)(double x);
using PROC_jl_cstr_to_string = jl_value_t* (*)(const char* str);
using PROC_jl_apply_array_type = jl_value_t* (*)(jl_value_t* type, size_t dim);
using PROC_jl_alloc_array_1d = jl_array_t* (*)(jl_value_t* atype, size_t nr);
using PROC_jl_alloc_array_2d = jl_array_t* (*)(jl_value_t* atype, size_t nr, size_t nc);
using PROC_jl_alloc_array_3d = jl_array_t* (*)(jl_value_t* atype, size_t nr, size_t nc, size_t z);
using PROC_jl_alloc_array_nd = jl_array_t* (*)(jl_value_t* atype, size_t* dims, size_t ndims);
using PROC_jl_get_nth_field = jl_value_t* (*)(jl_value_t* v, size_t i);
//=============================================================================
bool
isJuliaLibraryLoaded()
{
    return juliaLibraryLoaded;
}
//=============================================================================
bool
loadJuliaLibrary(const std::wstring& juliaLibraryPathName)
{
    if (juliaLibraryLoaded) {
        return true;
    }
#ifdef _MSC_VER
    julia_handle = Nelson::load_dynamic_libraryW(juliaLibraryPathName);
#else
    julia_handle = Nelson::load_dynamic_library(Nelson::wstring_to_utf8(juliaLibraryPathName));
#endif
    if (!julia_handle) {
        return false;
    }
    juliaLibraryLoaded = loadJuliaSymbols();
    return juliaLibraryLoaded;
}
//=============================================================================
bool
unloadJuliaLibrary()
{
    juliaSymbols.clear();
    juliaLibraryLoaded = false;
    return true;
}
//=============================================================================
bool
loadJuliaSymbols()
{
    //=============================================================================
    // Types
    //=============================================================================

    //=============================================================================
    // Functions
    //=============================================================================
    LOAD_JULIA_SYMBOL(jl_init);
    LOAD_JULIA_SYMBOL(jl_eval_string);
    LOAD_JULIA_SYMBOL(jl_exception_occurred);
    LOAD_JULIA_SYMBOL(jl_typeof_str);
    LOAD_JULIA_SYMBOL(jl_string_ptr);
    LOAD_JULIA_SYMBOL(jl_exception_clear);
    LOAD_JULIA_SYMBOL(jl_get_global);
    LOAD_JULIA_SYMBOL(jl_set_global);
    LOAD_JULIA_SYMBOL(jl_unbox_bool);
    LOAD_JULIA_SYMBOL(jl_unbox_float32);
    LOAD_JULIA_SYMBOL(jl_unbox_float64);
    LOAD_JULIA_SYMBOL(jl_unbox_int8);
    LOAD_JULIA_SYMBOL(jl_unbox_uint8);
    LOAD_JULIA_SYMBOL(jl_unbox_int16);
    LOAD_JULIA_SYMBOL(jl_unbox_uint16);
    LOAD_JULIA_SYMBOL(jl_unbox_int32);
    LOAD_JULIA_SYMBOL(jl_unbox_uint32);
    LOAD_JULIA_SYMBOL(jl_unbox_int64);
    LOAD_JULIA_SYMBOL(jl_unbox_uint64);
    LOAD_JULIA_SYMBOL(jl_symbol);
    LOAD_JULIA_SYMBOL(jl_array_eltype);
    LOAD_JULIA_SYMBOL(jl_get_field);
    LOAD_JULIA_SYMBOL(jl_field_index);
    LOAD_JULIA_SYMBOL(jl_get_nth_field);
    LOAD_JULIA_SYMBOL(jl_call);
    LOAD_JULIA_SYMBOL(jl_call0);
    LOAD_JULIA_SYMBOL(jl_call1);
    LOAD_JULIA_SYMBOL(jl_call2);
    LOAD_JULIA_SYMBOL(jl_call3);
    LOAD_JULIA_SYMBOL(jl_box_bool);
    LOAD_JULIA_SYMBOL(jl_box_int8);
    LOAD_JULIA_SYMBOL(jl_box_uint8);
    LOAD_JULIA_SYMBOL(jl_box_int16);
    LOAD_JULIA_SYMBOL(jl_box_uint16);
    LOAD_JULIA_SYMBOL(jl_box_int32);
    LOAD_JULIA_SYMBOL(jl_box_uint32);
    LOAD_JULIA_SYMBOL(jl_box_char);
    LOAD_JULIA_SYMBOL(jl_box_int64);
    LOAD_JULIA_SYMBOL(jl_box_uint64);
    LOAD_JULIA_SYMBOL(jl_box_float32);
    LOAD_JULIA_SYMBOL(jl_box_float64);
    LOAD_JULIA_SYMBOL(jl_cstr_to_string);
    LOAD_JULIA_SYMBOL(jl_apply_array_type);
    LOAD_JULIA_SYMBOL(jl_alloc_array_1d);
    LOAD_JULIA_SYMBOL(jl_alloc_array_2d);
    LOAD_JULIA_SYMBOL(jl_alloc_array_3d);
    LOAD_JULIA_SYMBOL(jl_alloc_array_nd);
    //=============================================================================
#undef LOAD_JULIA_SYMBOL
    return true;
}
//=============================================================================
void
NLSjl_init()
{
    reinterpret_cast<PROC_jl_init>(juliaSymbols["jl_init"])();
}
//=============================================================================
jl_value_t*
NLSjl_eval_string(const std::string str)
{
    return NLSjl_eval_string(str.c_str());
}
//=============================================================================
jl_value_t*
NLSjl_eval_string(const char* str)
{
    return reinterpret_cast<PROC_jl_eval_string>(juliaSymbols["jl_eval_string"])(str);
}
//=============================================================================
jl_value_t*
NLSjl_exception_occurred(void)
{
    return reinterpret_cast<PROC_jl_exception_occurred>(juliaSymbols["jl_exception_occurred"])();
}
//=============================================================================
void
NLSjl_exception_clear(void)
{
    reinterpret_cast<PROC_jl_exception_clear>(juliaSymbols["jl_exception_clear"])();
}
//=============================================================================
const char*
NLSjl_string_ptr(jl_value_t* s)
{
    return reinterpret_cast<PROC_jl_string_ptr>(juliaSymbols["jl_string_ptr"])(s);
}
//=============================================================================
void
NLSjl_set_global(jl_module_t* m, jl_sym_t* var, jl_value_t* val)
{
    reinterpret_cast<PROC_jl_set_global>(juliaSymbols["jl_set_global"])(m, var, val);
}
//=============================================================================
jl_value_t*
NLSjl_get_global(jl_module_t* m, jl_sym_t* var)
{
    return reinterpret_cast<PROC_jl_get_global>(juliaSymbols["jl_get_global"])(m, var);
}
//=============================================================================
int8_t
NLSjl_unbox_bool(jl_value_t* v)
{
    return (int8_t) reinterpret_cast<PROC_jl_unbox_bool>(juliaSymbols["jl_unbox_bool"])(v);
}
//=============================================================================
float
NLSjl_unbox_float32(jl_value_t* v)
{
    return (float)reinterpret_cast<PROC_jl_unbox_float32>(juliaSymbols["jl_unbox_float32"])(v);
}
//=============================================================================
double
NLSjl_unbox_float64(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_float64>(juliaSymbols["jl_unbox_float64"])(v);
}
//=============================================================================
int8_t
NLSjl_unbox_int8(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_int8>(juliaSymbols["jl_unbox_int8"])(v);
}
//=============================================================================
uint8_t
NLSjl_unbox_uint8(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_uint8>(juliaSymbols["jl_unbox_uint8"])(v);
}
//=============================================================================
int16_t
NLSjl_unbox_int16(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_int16>(juliaSymbols["jl_unbox_int16"])(v);
}
//=============================================================================
uint16_t
NLSjl_unbox_uint16(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_uint16>(juliaSymbols["jl_unbox_uint16"])(v);
}
//=============================================================================
int32_t
NLSjl_unbox_int32(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_int32>(juliaSymbols["jl_unbox_int32"])(v);
}
//=============================================================================
uint32_t
NLSjl_unbox_uint32(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_uint32>(juliaSymbols["jl_unbox_uint32"])(v);
}
//=============================================================================
int64_t
NLSjl_unbox_int64(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_int64>(juliaSymbols["jl_unbox_int64"])(v);
}
//=============================================================================
uint64_t
NLSjl_unbox_uint64(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_unbox_uint64>(juliaSymbols["jl_unbox_uint64"])(v);
}
//=============================================================================
jl_sym_t*
NLSjl_symbol(const char* str)
{
    return reinterpret_cast<PROC_jl_symbol>(juliaSymbols["jl_symbol"])(str);
}
//=============================================================================
const char*
NLSjl_typeof_str(jl_value_t* v)
{
    return reinterpret_cast<PROC_jl_typeof_str>(juliaSymbols["jl_typeof_str"])(v);
}
//=============================================================================
void*
NLSjl_array_eltype(jl_value_t* a)
{
    return reinterpret_cast<PROC_jl_array_eltype>(juliaSymbols["jl_array_eltype"])(a);
}
//=============================================================================
jl_value_t*
NLSjl_get_field(jl_value_t* o, const char* fld)
{
    return reinterpret_cast<PROC_jl_get_field>(juliaSymbols["jl_get_field"])(o, fld);
}
//=============================================================================
int
NLSjl_field_index(jl_datatype_t* t, jl_sym_t* fld, int err)
{
    return reinterpret_cast<PROC_jl_field_index>(juliaSymbols["jl_field_index"])(t, fld, err);
}
//=============================================================================
jl_value_t*
NLSjl_call(jl_function_t* f, jl_value_t** args, uint32_t nargs)
{
    return reinterpret_cast<PROC_jl_call>(juliaSymbols["jl_call"])(f, args, nargs);
}
//=============================================================================
jl_value_t*
NLSjl_call0(jl_function_t* f)
{
    return reinterpret_cast<PROC_jl_call0>(juliaSymbols["jl_call0"])(f);
}
//=============================================================================
jl_value_t*
NLSjl_call1(jl_function_t* f, jl_value_t* a)
{
    return reinterpret_cast<PROC_jl_call1>(juliaSymbols["jl_call1"])(f, a);
}
//=============================================================================
jl_value_t*
NLSjl_call2(jl_function_t* f, jl_value_t* a, jl_value_t* b)
{
    return reinterpret_cast<PROC_jl_call2>(juliaSymbols["jl_call2"])(f, a, b);
}
//=============================================================================
jl_value_t*
NLSjl_call3(jl_function_t* f, jl_value_t* a, jl_value_t* b, jl_value_t* c)
{
    return reinterpret_cast<PROC_jl_call3>(juliaSymbols["jl_call3"])(f, a, b, c);
}
//=============================================================================
jl_function_t*
NLSjl_get_function(jl_module_t* m, const char* name)
{
    return (jl_function_t*)NLSjl_get_global(m, NLSjl_symbol(name));
}
//=============================================================================
jl_value_t*
NLSjl_box_logical(uint8_t x)
{
    return NLSjl_box_bool(x != 0);
}
//=============================================================================
jl_value_t*
NLSjl_box_bool(int8_t x)
{
    return reinterpret_cast<PROC_jl_box_bool>(juliaSymbols["jl_box_bool"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_int8(int8_t x)
{
    return reinterpret_cast<PROC_jl_box_int8>(juliaSymbols["jl_box_int8"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_uint8(uint8_t x)
{
    return reinterpret_cast<PROC_jl_box_uint8>(juliaSymbols["jl_box_uint8"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_int16(int16_t x)
{
    return reinterpret_cast<PROC_jl_box_int16>(juliaSymbols["jl_box_int16"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_uint16(uint16_t x)
{
    return reinterpret_cast<PROC_jl_box_uint16>(juliaSymbols["jl_box_uint16"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_int32(int32_t x)
{
    return reinterpret_cast<PROC_jl_box_int32>(juliaSymbols["jl_box_int32"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_uint32(uint32_t x)
{
    return reinterpret_cast<PROC_jl_box_uint32>(juliaSymbols["jl_box_uint32"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_char(uint32_t x)
{
    return reinterpret_cast<PROC_jl_box_char>(juliaSymbols["jl_box_char"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_int64(int64_t x)
{
    return reinterpret_cast<PROC_jl_box_int64>(juliaSymbols["jl_box_int64"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_uint64(uint64_t x)
{
    return reinterpret_cast<PROC_jl_box_uint64>(juliaSymbols["jl_box_uint64"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_float32(float x)
{
    return reinterpret_cast<PROC_jl_box_float32>(juliaSymbols["jl_box_float32"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_box_float64(double x)
{
    return reinterpret_cast<PROC_jl_box_float64>(juliaSymbols["jl_box_float64"])(x);
}
//=============================================================================
jl_value_t*
NLSjl_cstr_to_string(const std::string& str)
{
    return NLSjl_cstr_to_string(str.c_str());
}
//=============================================================================
jl_value_t*
NLSjl_cstr_to_string(const char* str)
{
    return reinterpret_cast<PROC_jl_cstr_to_string>(juliaSymbols["jl_cstr_to_string"])(str);
}
//=============================================================================
jl_value_t*
NLSjl_apply_array_type(jl_value_t* type, size_t dim)
{
    return reinterpret_cast<PROC_jl_apply_array_type>(juliaSymbols["jl_apply_array_type"])(
        type, dim);
}
//=============================================================================
jl_array_t*
NLSjl_alloc_array_1d(jl_value_t* atype, size_t nr)
{
    return reinterpret_cast<PROC_jl_alloc_array_1d>(juliaSymbols["jl_alloc_array_1d"])(atype, nr);
}
//=============================================================================
jl_array_t*
NLSjl_alloc_array_2d(jl_value_t* atype, size_t nr, size_t nc)
{
    return reinterpret_cast<PROC_jl_alloc_array_2d>(juliaSymbols["jl_alloc_array_2d"])(
        atype, nr, nc);
}
//=============================================================================
jl_array_t*
NLSjl_alloc_array_3d(jl_value_t* atype, size_t nr, size_t nc, size_t z)
{
    return reinterpret_cast<PROC_jl_alloc_array_3d>(juliaSymbols["jl_alloc_array_3d"])(
        atype, nr, nc, z);
}
//=============================================================================
jl_array_t*
NLSjl_alloc_array_nd(jl_value_t* atype, size_t* dims, size_t ndims)
{
    return reinterpret_cast<PROC_jl_alloc_array_nd>(juliaSymbols["jl_alloc_array_nd"])(
        atype, dims, ndims);
}
//=============================================================================
jl_value_t*
NLSjl_get_nth_field(jl_value_t* v, size_t i)
{
    return reinterpret_cast<PROC_jl_get_nth_field>(juliaSymbols["jl_get_nth_field"])(v, i);
}
//=============================================================================
