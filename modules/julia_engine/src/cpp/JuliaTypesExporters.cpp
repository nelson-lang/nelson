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
#include <Eigen/Sparse>
#include "JuliaTypesHelpers.hpp"
#include "JuliaObjectHandle.hpp"
#include "characters_encoding.hpp"
#include "JuliaHelpers.hpp"
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
            return ArrayOf::logicalConstructor(NLSjl_unbox_bool(value) != 0 ? true : false);
        } },
    { JL_FLOAT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::singleConstructor(NLSjl_unbox_float32(value)); } },
    { JL_FLOAT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::doubleConstructor(NLSjl_unbox_float64(value)); } },
    { JL_INT8_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int8Constructor(NLSjl_unbox_int8(value)); } },
    { JL_INT16_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int16Constructor(NLSjl_unbox_int16(value)); } },
    { JL_INT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int32Constructor(NLSjl_unbox_int32(value)); } },
    { JL_INT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::int64Constructor(NLSjl_unbox_int64(value)); } },
    { JL_UINT8_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint8Constructor(NLSjl_unbox_uint8(value)); } },
    { JL_UINT16_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint16Constructor(NLSjl_unbox_uint16(value)); } },
    { JL_UINT32_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint32Constructor(NLSjl_unbox_uint32(value)); } },
    { JL_UINT64_TYPE_STR,
        [](jl_value_t* value) { return ArrayOf::uint64Constructor(NLSjl_unbox_uint64(value)); } },
    { JL_STRING_TYPE_STR,
        [](jl_value_t* value) {
            const char* str_data = NLSjl_string_ptr(value);
            std::string ustr;
            if (str_data) {
                ustr = std::string(str_data);
            }
            return ArrayOf::characterArrayConstructor(utf8_to_wstring(ustr));
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
            return ArrayOf::dcomplexConstructor(re, im);
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
            return ArrayOf::complexConstructor(re, im);
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
            return ArrayOf::dcomplexConstructor((double)re, (double)im);
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
    return ArrayOf::handleConstructor(juliaObjectHandle);
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

    std::vector<Eigen::Triplet<std::complex<T>>> tripletList;
    tripletList.reserve(nnz);

    std::complex<T>* jl_data = (std::complex<T>*)jl_array_data_(nzval);
    for (indexType j = 0; j < colPtrLen - 1; j++) {
        indexType start = colPointers[j] - 1;
        indexType end = colPointers[j + 1] - 1;
        for (indexType k = start; k < end; k++) {
            tripletList.emplace_back(rowIndices[k] - 1, j, jl_data[k]);
        }
    }

    void* spdata = nullptr;
    try {
        auto* spMat = new Eigen::SparseMatrix<std::complex<T>, Eigen::ColMajor, signedIndexType>(
            rows, cols);
        spMat->setFromTriplets(tripletList.begin(), tripletList.end());
        spMat->makeCompressed();
        spdata = static_cast<void*>(spMat);
    } catch (const std::bad_alloc&) {
        wasConverted = false;
        return ArrayOf();
    }

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
            I[idx] = rowIndices[k] - 1;
            J[idx] = j;
            V[idx] = values[k];
            idx++;
        }
    }

    void* spdata = nullptr;
    try {
        std::vector<Eigen::Triplet<T>> tripletList;
        tripletList.reserve(nnz);
        for (indexType k = 0; k < nnz; ++k) {
            tripletList.push_back(Eigen::Triplet<T>(I[k], J[k], V[k]));
        }
        auto* spMat = new Eigen::SparseMatrix<T, Eigen::ColMajor, signedIndexType>(rows, cols);
        spMat->setFromTriplets(tripletList.begin(), tripletList.end());
        spMat->makeCompressed();
        spdata = static_cast<void*>(spMat);
    } catch (const std::bad_alloc&) {
        wasConverted = false;
        return ArrayOf();
    }

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
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, dims.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dims, ptr);

    std::memcpy(ptr, data, dims.getElementCount() * sizeof(T) * 2);

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
    T* ptr = (T*)ArrayOf::allocateArrayOf(nelsonType, dims.getElementCount());
    ArrayOf res = ArrayOf(nelsonType, dims, ptr);
    std::memcpy(ptr, data, dims.getElementCount() * sizeof(T));
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
    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_STRING_ARRAY, dims.getElementCount());
    ArrayOf res = ArrayOf(NLS_STRING_ARRAY, dims, ptr);

    jl_array_t* arr = (jl_array_t*)value;
    jl_value_t** data = (jl_value_t**)jl_array_data_(arr);

    for (size_t i = 0; i < dims.getElementCount(); i++) {
        if (data[i] != nullptr) {
            const char* str = NLSjl_string_ptr(data[i]);
            ptr[i] = ArrayOf::characterArrayConstructor(std::string(str));
        } else {
            ptr[i] = ArrayOf::doubleConstructor(std::nan("NaN"));
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
        dimsVector = { static_cast<indexType>(elementCount), 1 }; // Convert set to Nx1 cell array

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
    } else { // isArray
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

    ArrayOf* ptr = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.getElementCount());
    ArrayOf res = ArrayOf(NLS_CELL_ARRAY, dims, ptr);

    for (size_t i = 0; i < dims.getElementCount(); i++) {
        if (data[i]) {
            ptr[i] = jl_value_tToArrayOf(data[i]);
        } else {
            ptr[i] = ArrayOf::emptyConstructor();
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
}
//=============================================================================
