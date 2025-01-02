//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#pragma warning(disable : 4018)
#endif
//=============================================================================
#include <Eigen/Sparse>
#include "JuliaTypesHelpers.hpp"
#include "JuliaObjectHandle.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename T>
jl_value_t*
NLSjl_createScalarComplexFloat(T realPart, T imagPart, const std::string& jlType)
{
    jl_value_t* (*boxFunction)(T v) = nullptr;
    if (jlType == JL_COMPLEX_FLOAT64_TYPE_LONG_STR) {
        if constexpr (std::is_same_v<T, double>) {
            boxFunction = NLSjl_box_float64;
        }
    }
    if (jlType == JL_COMPLEX_FLOAT32_TYPE_LONG_STR) {
        if constexpr (std::is_same_v<T, single>) {
            boxFunction = NLSjl_box_float32;
        }
    }
    if (jlType == JL_COMPLEX_INT64_TYPE_LONG_STR) {
        if constexpr (std::is_same_v<T, int64>) {
            boxFunction = NLSjl_box_int64;
        }
    }
    if (boxFunction == nullptr) {
        return nullptr;
    }

    jl_value_t* complexType = NLSjl_eval_string(jlType);
    jl_value_t* realValue = boxFunction(realPart);
    jl_value_t* imagValue = boxFunction(imagPart);
    jl_value_t* complexValue = NLSjl_eval_string("nothing");
    jl_value_t* complexValuePtr = nullptr;
    if (complexType && realValue && imagValue) {
        jl_value_t* args[2] = { realValue, imagValue };
        complexValue = NLSjl_call2(complexType, args[0], args[1]);
        complexValuePtr = complexValue;
    }
    return complexValuePtr;
}
//=============================================================================
static jl_value_t*
createEmptyJlArray(const std::string& jlTypeOfString, Dimensions dims)
{
    jl_value_t* valuePtr = nullptr;
    jl_value_t* julia_type = NLSjl_eval_string(jlTypeOfString);
    indexType len = dims.getLength();
    switch (len) {
    case 0:
    case 1:
    case 2: {
        jl_datatype_t* julia_matrix_type
            = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)julia_type, 2);
        jl_array_t* julia_matrix = (jl_array_t*)NLSjl_alloc_array_2d(
            (jl_value_t*)julia_matrix_type, dims.getRows(), dims.getColumns());
        valuePtr = (jl_value_t*)julia_matrix;
    } break;
    case 3: {
        jl_datatype_t* julia_matrix_type
            = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)julia_type, 3);
        jl_array_t* julia_matrix = (jl_array_t*)NLSjl_alloc_array_3d(
            (jl_value_t*)julia_matrix_type, dims.getRows(), dims.getColumns(), dims.getAt(2));
        valuePtr = (jl_value_t*)julia_matrix;
    } break;
    default: {
        jl_datatype_t* julia_matrix_type
            = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)julia_type, len);
        jl_array_t* julia_matrix = (jl_array_t*)NLSjl_alloc_array_nd(
            (jl_value_t*)julia_matrix_type, (size_t*)dims.getAsVector().data(), len);
        valuePtr = (jl_value_t*)julia_matrix;
    } break;
    }
    return valuePtr;
}
//=============================================================================
template <typename T> struct jl_complex_t
{
    T re;
    T im;
};
//=============================================================================
template <typename T>
static jl_value_t*
convertComplexArrayOfToJulia(const ArrayOf& value, const std::string& jlTypeOfString)
{
    jl_value_t* valuePtr = nullptr;
    Dimensions dims = value.getDimensions();
    T* ptr = (T*)(value.getDataPointer());
    size_t numElements = dims.getElementCount();
    jl_value_t* complex_type = NLSjl_eval_string(jlTypeOfString);
    jl_value_t* array_type = NLSjl_apply_array_type((jl_value_t*)complex_type, dims.getLength());
    valuePtr = (jl_value_t*)NLSjl_alloc_array_nd(
        array_type, (size_t*)dims.getAsVector().data(), dims.getLength());
    jl_complex_t<T>* data = (jl_complex_t<T>*)jl_array_data(valuePtr, jl_complex_t<T>);
    memcpy(data, ptr, sizeof(T) * 2 * numElements);
    return (jl_value_t*)valuePtr;
}
//=============================================================================
template <typename T>
static jl_value_t*
convertRealArrayOfToJulia(const ArrayOf& value, const std::string& jlTypeOfString)
{
    jl_value_t* valuePtr = nullptr;
    Dimensions dims = value.getDimensions();
    T* elements = (T*)(value.getDataPointer());
    jl_value_t* elements_type = NLSjl_eval_string(jlTypeOfString);
    size_t numElements = dims.getElementCount();

    jl_datatype_t* elements_matrix_type
        = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)elements_type, dims.getLength());
    jl_array_t* julia_array = (jl_array_t*)NLSjl_alloc_array_nd(
        (jl_value_t*)elements_matrix_type, (size_t*)dims.getAsVector().data(), dims.getLength());
    T* julia_data = jl_array_data(julia_array, T);
    memcpy(julia_data, elements, numElements * sizeof(T));
    valuePtr = (jl_value_t*)julia_array;
    return valuePtr;
}
//=============================================================================
template <typename T>
static jl_value_t*
convertSparseComplexArrayOfToJulia(const ArrayOf& value, const std::string& jlTypeOfString)
{
    Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>*)value.getSparseDataPointer();

    NLSjl_eval_string("using SparseArrays");
    jl_module_t* sparse_module = (jl_module_t*)NLSjl_eval_string("SparseArrays");
    jl_function_t* sparse_f = NLSjl_get_function(sparse_module, "sparse");

    typedef Eigen::Triplet<std::complex<T>> SparseTriplet;
    std::vector<SparseTriplet> triplets;

    for (int k = 0; k < spMat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<std::complex<T>, 0, signedIndexType>::InnerIterator it(
                 *spMat, k);
             it; ++it) {
            triplets.push_back(SparseTriplet(it.row() + 1, it.col() + 1, it.value()));
        }
    }

    jl_value_t* jl_int64_type = NLSjl_eval_string(JL_INT64_TYPE_STR);
    jl_value_t* jl_T_type = NLSjl_eval_string(jlTypeOfString);

    const size_t nnz = triplets.size();

    // Allocate Julia arrays for row indices, column indices, and real/imaginary values
    jl_value_t* I
        = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_int64_type, 1), nnz);
    jl_value_t* J
        = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_int64_type, 1), nnz);
    jl_value_t* V = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_T_type, 1), nnz);

    int64_t* I_data = (int64_t*)jl_array_data_(I);
    int64_t* J_data = (int64_t*)jl_array_data_(J);
    std::complex<T>* V_data = (std::complex<T>*)jl_array_data_(V);

    for (size_t i = 0; i < nnz; ++i) {
        I_data[i] = triplets[i].row();
        J_data[i] = triplets[i].col();
        V_data[i] = triplets[i].value();
    }

    // Call Julia sparse function
    jl_value_t* args[]
        = { I, J, V, NLSjl_box_int64(spMat->rows()), NLSjl_box_int64(spMat->cols()) };
    return NLSjl_call(sparse_f, args, 5);
}
//=============================================================================
template <typename T>
static jl_value_t*
convertSparseRealArrayOfToJulia(const ArrayOf& value, const std::string& jlTypeOfString)
{
    Eigen::SparseMatrix<T, 0, signedIndexType>* spMat
        = (Eigen::SparseMatrix<T, 0, signedIndexType>*)value.getSparseDataPointer();
    NLSjl_eval_string("using SparseArrays");
    jl_module_t* sparse_module = (jl_module_t*)NLSjl_eval_string("SparseArrays");
    jl_function_t* sparse_f = NLSjl_get_function(sparse_module, "sparse");
    typedef Eigen::Triplet<T> SparseTriplet;
    std::vector<SparseTriplet> triplets;
    for (int k = 0; k < spMat->outerSize(); ++k) {
        for (typename Eigen::SparseMatrix<T, 0, signedIndexType>::InnerIterator it(*spMat, k); it;
             ++it) {
            triplets.push_back(SparseTriplet(it.row() + 1, it.col() + 1, it.value()));
        }
    }
    jl_value_t* jl_int64_type = NLSjl_eval_string(JL_INT64_TYPE_STR);
    jl_value_t* jl_T_type = NLSjl_eval_string(jlTypeOfString);

    const size_t nnz = triplets.size();
    jl_value_t* I
        = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_int64_type, 1), nnz);
    jl_value_t* J
        = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_int64_type, 1), nnz);
    jl_value_t* V = (jl_value_t*)NLSjl_alloc_array_1d(NLSjl_apply_array_type(jl_T_type, 1), nnz);

    int64_t* I_data = (int64_t*)jl_array_data_(I);
    int64_t* J_data = (int64_t*)jl_array_data_(J);
    T* V_data = (T*)jl_array_data_(V);

    for (size_t i = 0; i < nnz; ++i) {
        I_data[i] = triplets[i].row();
        J_data[i] = triplets[i].col();
        V_data[i] = triplets[i].value();
    }
    jl_value_t* args[]
        = { I, J, V, NLSjl_box_int64(spMat->rows()), NLSjl_box_int64(spMat->cols()) };
    return NLSjl_call(sparse_f, args, 5);
}
//=============================================================================
static jl_value_t*
convertDictionaryArrayOfToJulia(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    stringVector fieldnames = value.getFieldNames();
    auto it = std::find(fieldnames.begin(), fieldnames.end(), "map");
    if (it == fieldnames.end()) {
        return nullptr;
    }
    it = std::find(fieldnames.begin(), fieldnames.end(), "allKeys");
    if (it == fieldnames.end()) {
        return nullptr;
    }
    ArrayOf map = value.getField("map");
    stringVector mapHash = map.getFieldNames();
    ArrayOf allKeys = value.getField("allKeys");
    if (allKeys.isCell() || allKeys.isStringArray()) {
        ArrayOf* element = (ArrayOf*)allKeys.getDataPointer();
        indexType nbElements = allKeys.getElementCount();

        jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
        jl_function_t* dict_constructor = NLSjl_get_function(jl_base_module, JL_DICT_TYPE_STR);
        valuePtr = NLSjl_call0(dict_constructor);
        jl_function_t* setindex_func = NLSjl_get_function(jl_base_module, "setindex!");

        for (indexType k = 0; k < nbElements; ++k) {
            jl_value_t* key = ArrayOfTojl_value_t(element[k]);
            jl_value_t* val = ArrayOfTojl_value_t(map.getField(mapHash[k]));
            NLSjl_call3(setindex_func, valuePtr, val, key);
        }
    }
    return valuePtr;
}
//=============================================================================
static jl_value_t*
convertStructScalarArrayOfToJulia(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    Dimensions dims = value.getDimensions();
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    jl_function_t* dict_constructor = NLSjl_get_function(jl_base_module, "Dict");
    valuePtr = NLSjl_call0(dict_constructor);
    jl_function_t* setindex_func = NLSjl_get_function(jl_base_module, "setindex!");

    stringVector fieldnames = value.getFieldNames();
    for (size_t k = 0; k < fieldnames.size(); ++k) {
        jl_value_t* key = NLSjl_cstr_to_string(fieldnames[k].c_str());
        jl_value_t* val = ArrayOfTojl_value_t(value.getField(fieldnames[k]));
        NLSjl_call3(setindex_func, valuePtr, val, key);
    }
    return valuePtr;
}
//=============================================================================
static jl_value_t*
convertStructArrayOfToJulia(const ArrayOf& value)
{
    jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
    jl_value_t* valuePtr = nullptr;
    Dimensions dims = value.getDimensions();
    jl_value_t* dict_type = NLSjl_eval_string("Dict");
    jl_value_t* array_type = NLSjl_apply_array_type(dict_type, dims.getLength());
    jl_array_t* dict_array = (jl_array_t*)NLSjl_alloc_array_nd(
        array_type, (size_t*)dims.getAsVector().data(), dims.getLength());

    jl_function_t* dict_constructor = NLSjl_get_function(jl_base_module, "Dict");
    jl_function_t* setindex_func = NLSjl_get_function(jl_base_module, "setindex!");

    stringVector fieldnames = value.getFieldNames();

    // Create dictionary for each element in the array
    jl_value_t** data = (jl_value_t**)jl_array_data_(dict_array);
    for (size_t i = 0; i < dims.getElementCount(); ++i) {
        data[i] = NLSjl_call0(dict_constructor);
        for (size_t k = 0; k < fieldnames.size(); ++k) {
            ArrayOfVector fields = value.getFieldAsList(fieldnames[k]);
            jl_value_t* key = NLSjl_cstr_to_string(fieldnames[k]);
            jl_value_t* val = ArrayOfTojl_value_t(fields[i]);
            NLSjl_call3(setindex_func, data[i], val, key);
        }
    }

    valuePtr = (jl_value_t*)dict_array;
    return valuePtr;
}
//=============================================================================
static jl_value_t*
convertCellArrayOfToJulia(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    Dimensions dims = value.getDimensions();
    ArrayOf* elements = (ArrayOf*)value.getDataPointer();
    jl_value_t* any_type = NLSjl_eval_string(JL_ANY_TYPE_STR);
    jl_datatype_t* any_matrix_type
        = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)any_type, dims.getLength());
    jl_array_t* any_matrix = (jl_array_t*)NLSjl_alloc_array_nd(
        (jl_value_t*)any_matrix_type, (size_t*)dims.getAsVector().data(), dims.getLength());
    jl_value_t** data = (jl_value_t**)jl_array_data_(any_matrix);
    for (size_t k = 0; k < dims.getElementCount(); ++k) {
        data[k] = ArrayOfTojl_value_t(elements[k]);
    }
    valuePtr = (jl_value_t*)any_matrix;
    return valuePtr;
}
//=============================================================================
static jl_value_t*
convertStringArrayOfToJulia(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    if (value.isEmpty()) {
        valuePtr = createEmptyJlArray(JL_STRING_TYPE_STR, value.getDimensions());
    } else if (value.isScalar()) {
        std::string str = value.getContentAsCString();
        valuePtr = NLSjl_cstr_to_string(str.c_str());
    } else {
        Dimensions dims = value.getDimensions();
        ArrayOf* elements = (ArrayOf*)value.getDataPointer();
        jl_value_t* string_type = NLSjl_eval_string(JL_STRING_TYPE_STR);
        jl_datatype_t* string_matrix_type
            = (jl_datatype_t*)NLSjl_apply_array_type((jl_value_t*)string_type, dims.getLength());
        jl_array_t* string_matrix = (jl_array_t*)NLSjl_alloc_array_nd(
            (jl_value_t*)string_matrix_type, (size_t*)dims.getAsVector().data(), dims.getLength());
        jl_value_t** data = (jl_value_t**)jl_array_data_(string_matrix);
        for (size_t k = 0; k < dims.getElementCount(); ++k) {
            data[k] = NLSjl_cstr_to_string(elements[k].getContentAsCString());
        }
        valuePtr = (jl_value_t*)string_matrix;
    }
    return valuePtr;
}
//=============================================================================
jl_value_t*
ArrayOfTojl_value_t(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    switch (value.getDataClass()) {
    case NLS_LOGICAL: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_BOOL_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_bool(value.getContentAsLogicalScalar());
        } else {
            if (value.isSparse()) {
                valuePtr = convertSparseRealArrayOfToJulia<uint8_t>(value, JL_BOOL_TYPE_STR);
            } else {
                valuePtr = convertRealArrayOfToJulia<uint8_t>(value, JL_BOOL_TYPE_STR);
            }
        }
    } break;
    case NLS_SCOMPLEX: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_COMPLEX_FLOAT32_TYPE_SHORT_STR, value.getDimensions());
        } else if (value.isScalar()) {
            std::complex<single> cplx = value.getContentAsSingleComplexScalar();
            valuePtr = NLSjl_createScalarComplexFloat<single>(
                cplx.real(), cplx.imag(), JL_COMPLEX_FLOAT32_TYPE_LONG_STR);
        } else {
            valuePtr
                = convertComplexArrayOfToJulia<single>(value, JL_COMPLEX_FLOAT32_TYPE_LONG_STR);
        }
    } break;
    case NLS_SINGLE: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_FLOAT64_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_float32(value.getContentAsSingleScalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<single>(value, JL_FLOAT32_TYPE_STR);
        }
    } break;
    case NLS_DCOMPLEX: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_COMPLEX_FLOAT64_TYPE_SHORT_STR, value.getDimensions());
        } else if (value.isScalar()) {
            std::complex<double> cplx = value.getContentAsDoubleComplexScalar();
            valuePtr = NLSjl_createScalarComplexFloat<double>(
                cplx.real(), cplx.imag(), JL_COMPLEX_FLOAT64_TYPE_LONG_STR);
        } else {
            if (value.isSparse()) {
                valuePtr = convertSparseComplexArrayOfToJulia<double>(
                    value, JL_COMPLEX_FLOAT64_TYPE_LONG_STR);
            } else {
                valuePtr
                    = convertComplexArrayOfToJulia<double>(value, JL_COMPLEX_FLOAT64_TYPE_LONG_STR);
            }
        }
    } break;
    case NLS_DOUBLE: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_FLOAT64_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_float64(value.getContentAsDoubleScalar());
        } else {
            if (value.isSparse()) {
                valuePtr = convertSparseRealArrayOfToJulia<double>(value, JL_FLOAT64_TYPE_STR);
            } else {
                valuePtr = convertRealArrayOfToJulia<double>(value, JL_FLOAT64_TYPE_STR);
            }
        }
    } break;
    case NLS_INT8: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_INT8_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_int8(value.getContentAsInteger8Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<int8>(value, JL_INT8_TYPE_STR);
        }
    } break;
    case NLS_INT16: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_INT16_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_int16(value.getContentAsInteger16Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<int16>(value, JL_INT16_TYPE_STR);
        }
    } break;
    case NLS_INT32: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_INT32_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_int32(value.getContentAsInteger32Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<int32>(value, JL_INT32_TYPE_STR);
        }
    } break;
    case NLS_INT64: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_INT64_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_int64(value.getContentAsInteger64Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<int64>(value, JL_INT64_TYPE_STR);
        }
    } break;
    case NLS_UINT8: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_UINT8_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_uint8(value.getContentAsUnsignedInteger8Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<uint8>(value, JL_UINT8_TYPE_STR);
        }
    } break;
    case NLS_UINT16: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_UINT16_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_uint16(value.getContentAsUnsignedInteger16Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<uint16>(value, JL_UINT16_TYPE_STR);
        }
    } break;
    case NLS_UINT32: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_UINT32_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_uint32(value.getContentAsUnsignedInteger32Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<uint32>(value, JL_UINT32_TYPE_STR);
        }
    } break;
    case NLS_UINT64: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_UINT64_TYPE_STR, value.getDimensions());
        } else if (value.isScalar()) {
            valuePtr = NLSjl_box_uint64(value.getContentAsUnsignedInteger64Scalar());
        } else {
            valuePtr = convertRealArrayOfToJulia<uint64>(value, JL_UINT64_TYPE_STR);
        }
    } break;
    case NLS_CHAR: {
        std::string str = value.getContentAsCString();
        valuePtr = NLSjl_cstr_to_string(str.c_str());
    } break;
    case NLS_STRING_ARRAY: {
        valuePtr = convertStringArrayOfToJulia(value);
    } break;
    case NLS_CELL_ARRAY: {
        valuePtr = convertCellArrayOfToJulia(value);
    } break;
    case NLS_STRUCT_ARRAY: {
        if (value.isEmpty()) {
            valuePtr = createEmptyJlArray(JL_DICT_TYPE_STR, value.getDimensions());

        } else if (value.isScalar()) {
            valuePtr = convertStructScalarArrayOfToJulia(value);
        } else {
            valuePtr = convertStructArrayOfToJulia(value);
        }
    } break;
    case NLS_HANDLE: {
        if (value.getHandleCategory() == NLS_HANDLE_JULIA_CATEGORY_STR) {
            JuliaObjectHandle* joh = (JuliaObjectHandle*)value.getContentAsHandleScalar();
            if (joh) {
                valuePtr = (jl_value_t*)joh->getPointer();
            }
        }
    } break;
    case NLS_CLASS_ARRAY: {
        if (value.getClassType() == "dictionary") {
            valuePtr = convertDictionaryArrayOfToJulia(value);
        }
    } break;
    case NLS_FUNCTION_HANDLE:
    case NLS_GO_HANDLE:
    case NLS_UNKNOWN:
    default: {
    } break;
    }
    return valuePtr;
}
//=============================================================================
}
//=============================================================================
