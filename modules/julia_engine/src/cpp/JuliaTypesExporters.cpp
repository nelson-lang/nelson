//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <unordered_map>
#include <functional>
#include <cstring>
#include "JuliaTypesHelpers.hpp"
#include "JuliaObjectHandle.hpp"
#include "characters_encoding.hpp"
#include "JuliaHelpers.hpp"
#include "SparseType.hpp"
#include "StringHelpers.hpp"
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4359)
#pragma warning(disable : 4018)
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
const std::unordered_map<std::string, std::function<ArrayOf(jl_value_t*)>> TYPE_CONVERTERS = {
    { JL_BOOL_TYPE_STR,
        [](jl_value_t* value) {
            return ArrayOf::logicalScalar(NLSjl_unbox_bool(value) != 0 ? true : false);
        } },
    { JL_FLOAT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::singleScalar(NLSjl_unbox_float32(value)); } },
    { JL_FLOAT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::doubleScalar(NLSjl_unbox_float64(value)); } },
    { JL_INT8_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int8Scalar(NLSjl_unbox_int8(value)); } },
    { JL_INT16_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int16Scalar(NLSjl_unbox_int16(value)); } },
    { JL_INT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int32Scalar(NLSjl_unbox_int32(value)); } },
    { JL_INT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int64Scalar(NLSjl_unbox_int64(value)); } },
    { JL_UINT8_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint8Scalar(NLSjl_unbox_uint8(value)); } },
    { JL_UINT16_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint16Scalar(NLSjl_unbox_uint16(value)); } },
    { JL_UINT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint32Scalar(NLSjl_unbox_uint32(value)); } },
    { JL_UINT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint64Scalar(NLSjl_unbox_uint64(value)); } },
    { JL_STRING_TYPE_STR,
        [](jl_value_t* value) {
            const char* str_data = NLSjl_string_ptr(value);
            std::string ustr;
            if (str_data) {
                ustr = std::string(str_data);
            }
            return ArrayOf::characterArray(utf8_to_wstring(ustr));
        } },
    { JL_COMPLEX_FLOAT64_TYPE_SHORT_STR,
        [](jl_value_t* value) {
            double re = 0;
            double im = 0;
            jl_value_t* realPart = NLSjl_get_field(value, "re");

            if (realPart != nullptr) {
                re = NLSjl_unbox_float64(realPart);
            }
            jl_value_t* imagPart = NLSjl_get_field(value, "im");

            if (imagPart != nullptr) {
                im = NLSjl_unbox_float64(imagPart);
            }
            return ArrayOf::dsingleComplexScalar(re, im);
        } },
    { JL_COMPLEX_FLOAT32_TYPE_SHORT_STR,
        [](jl_value_t* value) {
            single re = 0;
            single im = 0;
            jl_value_t* realPart = NLSjl_get_field(value, "re");

            if (realPart != nullptr) {
                re = NLSjl_unbox_float32(realPart);
            }
            jl_value_t* imagPart = NLSjl_get_field(value, "im");

            if (imagPart != nullptr) {
                im = NLSjl_unbox_float32(imagPart);
            }
            return ArrayOf::singleComplexScalar(re, im);
        } },
    { JL_COMPLEX_INT64_TYPE_LONG_STR,
        [](jl_value_t* value) {
            int64 re = 0;
            int64 im = 0;
            jl_value_t* realPart = NLSjl_get_field(value, "re");

            if (realPart != nullptr) {
                re = NLSjl_unbox_int64(realPart);
            }
            jl_value_t* imagPart = NLSjl_get_field(value, "im");

            if (imagPart != nullptr) {
                im = NLSjl_unbox_int64(imagPart);
            }
            return ArrayOf::dsingleComplexScalar((double)re, (double)im);
        } },
};
//=============================================================================
ArrayOf
jl_value_tToArrayOf(jl_value_t* value)
{
    std::string type_name = jl_get_type_of_as_string(value);
    auto scalarConverter = TYPE_CONVERTERS.find(type_name);
    if (scalarConverter != TYPE_CONVERTERS.end()) {
        return scalarConverter->second(value);
    }
    JuliaObjectHandle* juliaObjectHandle = new JuliaObjectHandle(value);
    return ArrayOf::handle(juliaObjectHandle);
}
//=============================================================================
template <typename T>
ArrayOf
convertSparseMatrixComplexToArrayOf(jl_value_t* value, NelsonType nelsonType, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }

    jl_value_t* m = NLSjl_get_field(value, "m");
    jl_value_t* n = NLSjl_get_field(value, "n");
    if (!m || !n) {
        wasConverted = false;
        return ArrayOf();
    }

    indexType rows = NLSjl_unbox_int64(m);
    indexType cols = NLSjl_unbox_int64(n);

    jl_value_t* nzval = NLSjl_get_field(value, "nzval");
    jl_value_t* rowval = NLSjl_get_field(value, "rowval");
    jl_value_t* colptr = NLSjl_get_field(value, "colptr");

    if (!nzval || !rowval || !colptr) {
        wasConverted = false;
        return ArrayOf();
    }

    int64* rowIndices = (int64*)jl_array_data_(rowval);
    int64* colPointers = (int64*)jl_array_data_(colptr);
    indexType nnz = jl_array_len(nzval);
    indexType colPtrLen = jl_array_len(colptr);

    std::vector<indexType> I(nnz);
    std::vector<indexType> J(nnz);
    std::vector<doublecomplex> V(nnz);

    std::complex<T>* jl_data = (std::complex<T>*)jl_array_data_(nzval);
    indexType idx = 0;
    for (indexType j = 0; j < colPtrLen - 1; j++) {
        indexType start = colPointers[j] - 1;
        indexType end = colPointers[j + 1] - 1;
        for (indexType k = start; k < end; k++) {
            I[idx] = rowIndices[k];
            J[idx] = j + 1;
            V[idx] = jl_data[k];
            idx++;
        }
    }

    void* spdata = Eigen_makeSparseFromIJV(
        nelsonType, rows, cols, idx, I.data(), 1, J.data(), 1, V.data(), 1, false);
    if (spdata == nullptr) {
        wasConverted = false;
        return ArrayOf();
    }

    Dimensions dims(rows, cols);
    ArrayOf res = ArrayOf(nelsonType, dims, spdata, true);

    wasConverted = true;
    return res;
}
//=============================================================================
template <typename T>
ArrayOf
convertSparseMatrixRealToArrayOf(jl_value_t* value, NelsonType nelsonType, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }
    jl_value_t* m = NLSjl_get_field(value, "m");
    jl_value_t* n = NLSjl_get_field(value, "n");
    if (!m || !n) {
        wasConverted = false;
        return ArrayOf();
    }

    indexType rows = NLSjl_unbox_int64(m);
    indexType cols = NLSjl_unbox_int64(n);

    jl_value_t* nzval = NLSjl_get_field(value, "nzval");
    jl_value_t* rowval = NLSjl_get_field(value, "rowval");
    jl_value_t* colptr = NLSjl_get_field(value, "colptr");

    if (!nzval || !rowval || !colptr) {
        wasConverted = false;
        return ArrayOf();
    }

    T* values = (T*)jl_array_data_(nzval);
    int64* rowIndices = (int64*)jl_array_data_(rowval);
    int64* colPointers = (int64*)jl_array_data_(colptr);

    indexType nnz = jl_array_len(nzval);

    std::vector<indexType> I(nnz);
    std::vector<indexType> J(nnz);
    std::vector<T> V(nnz);

    indexType colPtrLen = jl_array_len(colptr);
    indexType idx = 0;
    for (indexType j = 0; j < colPtrLen - 1; j++) {
        indexType start = colPointers[j] - 1;
        indexType end = colPointers[j + 1] - 1;
        for (indexType k = start; k < end; k++) {
            I[idx] = rowIndices[k];
            J[idx] = j + 1;
            V[idx] = values[k];
            idx++;
        }
    }

    void* spdata = Eigen_makeSparseFromIJV(
        nelsonType, rows, cols, idx, I.data(), 1, J.data(), 1, V.data(), 1, false);
    if (spdata == nullptr) {
        wasConverted = false;
        return ArrayOf();
    }

    Dimensions dims(rows, cols);
    ArrayOf res = ArrayOf(nelsonType, dims, spdata, true);

    wasConverted = true;
    return res;
}
//=============================================================================
template <typename T>
ArrayOf
convertMatrixComplexToArrayOf(jl_value_t* value, NelsonType nelsonType, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }

    size_t ndims = jl_array_ndims(value);
    bool extendDims = false;
    if (ndims == 1) {
        ndims++;
        extendDims = true;
    }

    std::vector<indexType> dimsVector(ndims);
    for (size_t i = 0; i < ndims; i++) {
        dimsVector[i] = jl_array_dim(value, i);
    }
    if (extendDims) {
        dimsVector[ndims - 1] = 1;
    }

    Dimensions dims(dimsVector);
    T* data = (T*)jl_array_data_(value);
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, dims.elementCount());
    ArrayOf res = ArrayOf(nelsonType, dims, ptr);

    std::memcpy(ptr, data, dims.elementCount() * sizeof(T) * 2);

    wasConverted = true;
    return res;
}
//=============================================================================
template <typename T>
ArrayOf
convertMatrixRealToArrayOf(jl_value_t* value, NelsonType nelsonType, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }
    size_t ndims = jl_array_ndims(value);
    bool extendDims = false;
    if (ndims == 1) {
        ndims++;
        extendDims = true;
    }
    std::vector<indexType> dimsVector(ndims);
    for (size_t i = 0; i < ndims; i++) {
        dimsVector[i] = jl_array_dim(value, i);
    }
    if (extendDims) {
        dimsVector[ndims - 1] = 1;
    }
    T* data = (T*)jl_array_data_(value);
    Dimensions dims(dimsVector);
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, dims.elementCount());
    ArrayOf res = ArrayOf(nelsonType, dims, ptr);
    std::memcpy(ptr, data, dims.elementCount() * sizeof(T));
    wasConverted = true;
    return res;
}
//=============================================================================
ArrayOf
convertSparseMatrixComplexFloat64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertSparseMatrixComplexToArrayOf<double>(value, NLS_DCOMPLEX, wasConverted);
}
//=============================================================================
ArrayOf
convertSparseMatrixFloat64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertSparseMatrixRealToArrayOf<double>(value, NLS_DOUBLE, wasConverted);
}
//=============================================================================
ArrayOf
convertSparseMatrixFloat32ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertSparseMatrixRealToArrayOf<single>(value, NLS_SINGLE, wasConverted);
}
//=============================================================================
ArrayOf
convertSparseMatrixBoolToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertSparseMatrixRealToArrayOf<logical>(value, NLS_LOGICAL, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixComplexFloat64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixComplexToArrayOf<double>(value, NLS_DCOMPLEX, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixComplexFloat32ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixComplexToArrayOf<single>(value, NLS_SCOMPLEX, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixFloat64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<double>(value, NLS_DOUBLE, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixFloat32ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<single>(value, NLS_SINGLE, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixUInt8ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<uint8>(value, NLS_UINT8, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixUInt16ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<uint16>(value, NLS_UINT16, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixUInt32ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<uint32>(value, NLS_UINT32, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixUInt64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<uint64>(value, NLS_UINT64, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixInt8ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<int8>(value, NLS_INT8, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixInt16ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<int16>(value, NLS_INT16, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixInt32ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<int32>(value, NLS_INT32, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixInt64ToArrayOf(jl_value_t* value, bool& wasConverted)
{
    return convertMatrixRealToArrayOf<int64>(value, NLS_INT64, wasConverted);
}
//=============================================================================
ArrayOf
convertMatrixStringToArrayOf(jl_value_t* value, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }
    size_t ndims = jl_array_ndims(value);
    bool extendDims = false;
    if (ndims == 1) {
        ndims++;
        extendDims = true;
    }
    std::vector<indexType> dimsVector(ndims);
    for (size_t i = 0; i < ndims; i++) {
        dimsVector[i] = jl_array_dim(value, i);
    }
    if (extendDims) {
        dimsVector[ndims - 1] = 1;
    }
    Dimensions dims(dimsVector);
    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dims.elementCount());
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dims, ptr);

    jl_array_t* arr = (jl_array_t*)value;
    jl_value_t** data = (jl_value_t**)jl_array_data_(arr);

    for (size_t i = 0; i < dims.elementCount(); i++) {
        if (data[i] != nullptr) {
            const char* str = NLSjl_string_ptr(data[i]);
            ptr[i] = ArrayOf::characterArray(std::string(str));
        } else {
            ptr[i] = ArrayOf::doubleScalar(std::nan(""));
        }
    }
    wasConverted = true;
    return res;
}
//=============================================================================
ArrayOf
convertCellToArrayOf(jl_value_t* value, bool& wasConverted)
{
    if (!value) {
        wasConverted = false;
        return ArrayOf();
    }
    // `data` points into `value` (or the collect() result for a Set/Tuple), and the
    // per-element jl_value_tToArrayOf() below allocates as it recurses. Without the
    // collector held off, a collection could free the backing array mid-loop and
    // leave `data` dangling.
    NLSjlGcDisableGuard gcGuard;
    std::string typeOf = jl_get_type_of_as_string(value);
    bool isSet = StringHelpers::starts_with(typeOf, "Set{");
    bool isTuple = StringHelpers::starts_with(typeOf, "Tuple{");
    bool isArray = StringHelpers::starts_with(typeOf, "Array{");
    bool isVector = StringHelpers::starts_with(typeOf, "Vector{");
    bool isMatrix = StringHelpers::starts_with(typeOf, "Matrix{");

    if (!isTuple && !isArray && !isSet && !isVector && !isMatrix) {
        wasConverted = false;
        return ArrayOf();
    }

    size_t elementCount;
    jl_value_t** data;
    std::vector<indexType> dimsVector;

    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    jl_function_t* length_func = NLSjl_get_function(jl_base_module, "length");
    if (isTuple) {
        if (!length_func) {
            wasConverted = false;
            return ArrayOf();
        }

        jl_value_t* len = NLSjl_call1(length_func, value);
        if (!len) {
            wasConverted = false;
            return ArrayOf();
        }

#ifdef _P64
        elementCount = NLSjl_unbox_int64(len);
#else
        elementCount = NLSjl_unbox_int32(len);
#endif
        dimsVector = { static_cast<indexType>(elementCount), 1 };
        jl_function_t* collect_func = NLSjl_get_function(jl_base_module, "collect");
        if (!collect_func) {
            wasConverted = false;
            return ArrayOf();
        }

        jl_value_t* tuple_array = NLSjl_call1(collect_func, value);
        if (!tuple_array) {
            wasConverted = false;
            return ArrayOf();
        }

        data = (jl_value_t**)jl_array_data_(tuple_array);
    } else if (isSet) {
        jl_value_t* len = NLSjl_call1(length_func, value);
        if (!len) {
            wasConverted = false;
            return ArrayOf();
        }

#ifdef _P64
        elementCount = NLSjl_unbox_int64(len);
#else
        elementCount = NLSjl_unbox_int32(len);
#endif
        dimsVector = { static_cast<indexType>(elementCount), 1 };

        jl_function_t* collect_func = NLSjl_get_function(jl_base_module, "collect");
        if (!collect_func) {
            wasConverted = false;
            return ArrayOf();
        }

        jl_value_t* set_array = NLSjl_call1(collect_func, value);
        if (!set_array) {
            wasConverted = false;
            return ArrayOf();
        }

        data = (jl_value_t**)jl_array_data_(set_array);
    } else {
        size_t ndims = jl_array_ndims(value);
        bool extendDims = false;
        if (ndims == 1) {
            ndims++;
            extendDims = true;
        }
        dimsVector.resize(ndims);
        for (size_t i = 0; i < ndims; i++) {
            dimsVector[i] = jl_array_dim(value, i);
        }
        if (extendDims) {
            dimsVector[ndims - 1] = 1;
        }
        elementCount = 1;
        for (const auto& dim : dimsVector) {
            elementCount *= dim;
        }
        jl_array_t* arr = (jl_array_t*)value;
        data = (jl_value_t**)jl_array_data_(arr);
    }
    Dimensions dims(dimsVector);

    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount());
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, ptr);

    for (size_t i = 0; i < dims.elementCount(); i++) {
        if (data[i]) {
            ptr[i] = jl_value_tToArrayOf(data[i]);
        } else {
            ptr[i] = ArrayOf::empty();
        }
    }
    wasConverted = true;
    return res;
}
//=============================================================================
ArrayOf
jl_value_tGetProperty(jl_value_t* value, const std::string& propertyName, bool& wasFound)
{
    if (!value) {
        wasFound = false;
        return ArrayOf();
    }
    jl_value_t* m = NLSjl_get_field(value, propertyName.c_str());
    if (!m) {
        wasFound = false;
        return ArrayOf();
    }
    wasFound = true;
    return jl_value_tToArrayOf(m);
}
//=============================================================================
static ArrayOf
convertMatrixBoolToArrayOf(jl_value_t* value, bool& wasConverted)
{
    // A Julia Bool is a single byte holding 0 or 1, matching Nelson's logical
    // (uint8), so the generic byte-wise copy yields a logical column directly.
    return convertMatrixRealToArrayOf<logical>(value, NLS_LOGICAL, wasConverted);
}
//=============================================================================
static jl_function_t*
mainFunction(const char* name)
{
    jl_module_t* jl_main_module = (jl_module_t*)NLSjl_eval_string("Main");
    return jl_main_module ? NLSjl_get_function(jl_main_module, name) : nullptr;
}
//=============================================================================
static ArrayOf
juliaTextColumnToArrayOf(jl_value_t* column)
{
    // Stringify every element (missing -> "missing") and separately fetch the
    // missing mask, so masked rows become a <missing> gap (a non-char element,
    // stored as NaN) instead of the literal text "missing".
    jl_function_t* stringify_func = mainFunction("__nelson_stringify_col__");
    jl_function_t* mask_func = mainFunction("__nelson_col_missingmask__");
    if (!stringify_func) {
        return ArrayOf::empty();
    }
    jl_value_t* textColumn = NLSjl_call1(stringify_func, column);
    jl_value_t* maskColumn = mask_func ? NLSjl_call1(mask_func, column) : nullptr;
    if (!textColumn || NLSjl_exception_occurred()) {
        NLSjl_exception_clear();
        return ArrayOf::empty();
    }
    size_t nbRows = jl_array_len(textColumn);
    jl_value_t** textData = (jl_value_t**)jl_array_data_(textColumn);
    const uint8_t* maskData = maskColumn ? (const uint8_t*)jl_array_data_(maskColumn) : nullptr;
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, nbRows);
    ArrayOf res(NLS_STRING_ARRAY, Dimensions(nbRows, 1), elements);
    for (size_t k = 0; k < nbRows; ++k) {
        if (maskData && maskData[k]) {
            elements[k] = ArrayOf::doubleScalar(std::nan(""));
        } else {
            const char* str = textData[k] ? NLSjl_string_ptr(textData[k]) : nullptr;
            elements[k] = ArrayOf::characterArray(str ? std::string(str) : std::string());
        }
    }
    return res;
}
//=============================================================================
static ArrayOf
juliaColumnToArrayOf(jl_value_t* column)
{
    if (!column) {
        return ArrayOf::empty();
    }
    static const std::unordered_map<std::string, ArrayOf (*)(jl_value_t*, bool&)> COLUMN_CONVERTERS
        = {
              { "Vector{Bool}", convertMatrixBoolToArrayOf },
              { "Vector{Float64}", convertMatrixFloat64ToArrayOf },
              { "Vector{Float32}", convertMatrixFloat32ToArrayOf },
              { "Vector{ComplexF64}", convertMatrixComplexFloat64ToArrayOf },
              { "Vector{ComplexF32}", convertMatrixComplexFloat32ToArrayOf },
              { "Vector{Int8}", convertMatrixInt8ToArrayOf },
              { "Vector{Int16}", convertMatrixInt16ToArrayOf },
              { "Vector{Int32}", convertMatrixInt32ToArrayOf },
              { "Vector{Int64}", convertMatrixInt64ToArrayOf },
              { "Vector{UInt8}", convertMatrixUInt8ToArrayOf },
              { "Vector{UInt16}", convertMatrixUInt16ToArrayOf },
              { "Vector{UInt32}", convertMatrixUInt32ToArrayOf },
              { "Vector{UInt64}", convertMatrixUInt64ToArrayOf },
              { "Vector{String}", convertMatrixStringToArrayOf },
          };
    std::string typeOf = jl_get_type_of_as_string(column);
    bool wasConverted = false;
    auto it = COLUMN_CONVERTERS.find(typeOf);
    if (it != COLUMN_CONVERTERS.end()) {
        ArrayOf res = it->second(column, wasConverted);
        if (wasConverted) {
            return res;
        }
    }
    // A column whose element type is Union{Missing, T}: a numeric column becomes
    // double (or complex) with NaN for the missing entries; anything else becomes
    // a string column whose missing entries are <missing>.
    if (StringHelpers::starts_with(typeOf, "Vector{Union{")
        && typeOf.find("Missing") != std::string::npos) {
        jl_function_t* kind_func = mainFunction("__nelson_col_kind__");
        long long kind = 3;
        if (kind_func) {
            jl_value_t* kindValue = NLSjl_call1(kind_func, column);
            if (kindValue && !NLSjl_exception_occurred()) {
                kind = (long long)NLSjl_unbox_int64(kindValue);
            }
            if (NLSjl_exception_occurred()) {
                NLSjl_exception_clear();
            }
        }
        if (kind == 1 || kind == 2) {
            const char* materializer
                = (kind == 2) ? "__nelson_col_complexmissing__" : "__nelson_col_realmissing__";
            jl_function_t* materialize_func = mainFunction(materializer);
            if (materialize_func) {
                jl_value_t* numeric = NLSjl_call1(materialize_func, column);
                if (numeric && !NLSjl_exception_occurred()) {
                    ArrayOf res = (kind == 2)
                        ? convertMatrixComplexFloat64ToArrayOf(numeric, wasConverted)
                        : convertMatrixFloat64ToArrayOf(numeric, wasConverted);
                    if (wasConverted) {
                        return res;
                    }
                }
                if (NLSjl_exception_occurred()) {
                    NLSjl_exception_clear();
                }
            }
        }
        return juliaTextColumnToArrayOf(column);
    }
    // Any other column type (dates, categorical, ...) is exported as text.
    return juliaTextColumnToArrayOf(column);
}
//=============================================================================
static ArrayOf
cellFromArrayOfVector(const ArrayOfVector& values)
{
    size_t n = values.size();
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n);
    Dimensions dims(n == 0 ? 0 : 1, n);
    ArrayOf res(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < n; ++k) {
        elements[k] = values[k];
    }
    return res;
}
//=============================================================================
static ArrayOf
cellstrRowFromStringVector(const stringVector& values)
{
    size_t n = values.size();
    ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, n);
    Dimensions dims(n == 0 ? 0 : 1, n);
    ArrayOf res(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < n; ++k) {
        elements[k] = ArrayOf::characterArray(values[k]);
    }
    return res;
}
//=============================================================================
bool
JuliaDataFrameToTableData(
    jl_value_t* value, ArrayOf& columns, ArrayOf& variableNames, ArrayOf& rowNames)
{
    if (!isJuliaDataFrame(value) || !jl_ensure_dataframes()) {
        return false;
    }
    // The names vector, its element pointers, and each fetched column are unrooted
    // while the column converters below allocate; keep the collector off for the
    // whole extraction (also covers the static column helpers reached from here).
    NLSjlGcDisableGuard gcGuard;
    jl_module_t* jl_main_module = (jl_module_t*)NLSjl_eval_string("Main");
    if (!jl_main_module) {
        return false;
    }
    jl_function_t* names_func = NLSjl_get_function(jl_main_module, "__nelson_df_names__");
    jl_function_t* ncol_func = NLSjl_get_function(jl_main_module, "__nelson_df_ncol__");
    jl_function_t* getcol_func = NLSjl_get_function(jl_main_module, "__nelson_df_getcol__");
    if (!names_func || !ncol_func || !getcol_func) {
        return false;
    }

    jl_value_t* namesVector = NLSjl_call1(names_func, value);
    jl_value_t* ncolValue = NLSjl_call1(ncol_func, value);
    if (!namesVector || !ncolValue || NLSjl_exception_occurred()) {
        NLSjl_exception_clear();
        return false;
    }
    int64 nbColumns = NLSjl_unbox_int64(ncolValue);

    stringVector names;
    jl_value_t** namesData = (jl_value_t**)jl_array_data_(namesVector);
    size_t namesCount = jl_array_len(namesVector);
    for (size_t j = 0; j < namesCount; ++j) {
        const char* label = namesData[j] ? NLSjl_string_ptr(namesData[j]) : nullptr;
        names.push_back(label ? std::string(label) : std::string());
    }

    ArrayOfVector columnValues;
    for (int64 j = 1; j <= nbColumns; ++j) {
        jl_value_t* column = NLSjl_call2(getcol_func, value, NLSjl_box_int64(j));
        if (!column || NLSjl_exception_occurred()) {
            NLSjl_exception_clear();
            columnValues.push_back(ArrayOf::empty());
            continue;
        }
        columnValues.push_back(juliaColumnToArrayOf(column));
    }

    columns = cellFromArrayOfVector(columnValues);
    variableNames = cellstrRowFromStringVector(names);
    rowNames = cellstrRowFromStringVector(stringVector());
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
