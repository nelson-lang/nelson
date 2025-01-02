//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaTypesHelpers.hpp"
#include "JuliaObjectHandle.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define JL_BOOL_TYPE_STR "Bool"
#define JL_FLOAT32_TYPE_STR "Float32"
#define JL_FLOAT64_TYPE_STR "Float64"
#define JL_INT8_TYPE_STR "Int8"
#define JL_INT16_TYPE_STR "Int16"
#define JL_INT32_TYPE_STR "Int32"
#define JL_INT64_TYPE_STR "Int64"
#define JL_UINT8_TYPE_STR "UInt8"
#define JL_UINT16_TYPE_STR "UInt16"
#define JL_UINT32_TYPE_STR "UInt32"
#define JL_UINT64_TYPE_STR "UInt64"
#define JL_STRING_TYPE_STR "String"
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
};
//=============================================================================
ArrayOf
jl_value_tToArrayOf(jl_value_t* value)
{
    const char* type_name = NLSjl_typeof_str(value);
    auto scalarConverter = TYPE_CONVERTERS.find(type_name);
    if (scalarConverter != TYPE_CONVERTERS.end()) {
        return scalarConverter->second(value);
    }
    JuliaObjectHandle* juliaObjectHandle = new JuliaObjectHandle(value);
    return ArrayOf::handleConstructor(juliaObjectHandle);
}
//=============================================================================
jl_value_t*
ArrayOfTojl_value_t(const ArrayOf& value)
{
    jl_value_t* valuePtr = nullptr;
    switch (value.getDataClass()) {
    case NLS_LOGICAL: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_bool(value.getContentAsLogicalScalar());
        }
    } break;
    case NLS_SINGLE: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_float32(value.getContentAsSingleScalar());
        }
    } break;
    case NLS_DOUBLE: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_float64(value.getContentAsDoubleScalar());
        }
    } break;
    case NLS_INT8: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_int8(value.getContentAsInteger8Scalar());
        }
    } break;
    case NLS_INT16: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_int16(value.getContentAsInteger16Scalar());
        }
    } break;
    case NLS_INT32: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_int32(value.getContentAsInteger32Scalar());
        }
    } break;
    case NLS_INT64: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_int64(value.getContentAsInteger64Scalar());
        }
    } break;
    case NLS_UINT8: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_uint8(value.getContentAsUnsignedInteger8Scalar());
        }
    } break;
    case NLS_UINT16: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_uint16(value.getContentAsUnsignedInteger16Scalar());
        }
    } break;
    case NLS_UINT32: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_uint32(value.getContentAsUnsignedInteger32Scalar());
        }
    } break;
    case NLS_UINT64: {
        if (value.isScalar()) {
            valuePtr = NLSjl_box_uint64(value.getContentAsUnsignedInteger64Scalar());
        }
    } break;
    case NLS_CHAR: {
        std::string str = value.getContentAsCString();
        valuePtr = NLSjl_cstr_to_string(str.c_str());
    } break;
    }
    return valuePtr;
}
//=============================================================================
}
//=============================================================================
