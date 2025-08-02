//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <algorithm>
#include <functional>
#include <unordered_map>
#include "JuliaObjectHandle.hpp"
#include "JuliaTypesHelpers.hpp"
#include "JuliaHelpers.hpp"
#include "JuliaLibraryWrapper.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
#include "Error.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
JuliaObjectHandle::JuliaObjectHandle(void* _ptr)
    : HandleGenericObject(NLS_HANDLE_JULIA_CATEGORY_STR, _ptr, true)
{
    methodMap = { { L"cell",
                      std::bind(
                          &JuliaObjectHandle::invokeCastCellMethod, this, std::placeholders::_1) },
        { L"string",
            std::bind(&JuliaObjectHandle::invokeCastStringMethod, this, std::placeholders::_1) },
        { L"numeric",
            std::bind(&JuliaObjectHandle::invokeCastNumericMethod, this, std::placeholders::_1) },
        { L"double",
            std::bind(&JuliaObjectHandle::invokeCastDoubleMethod, this, std::placeholders::_1) },
        { L"single",
            std::bind(&JuliaObjectHandle::invokeCastSingleMethod, this, std::placeholders::_1) },
        { L"int8",
            std::bind(&JuliaObjectHandle::invokeCastInt8Method, this, std::placeholders::_1) },
        { L"int16",
            std::bind(&JuliaObjectHandle::invokeCastInt16Method, this, std::placeholders::_1) },
        { L"int32",
            std::bind(&JuliaObjectHandle::invokeCastInt32Method, this, std::placeholders::_1) },
        { L"int64",
            std::bind(&JuliaObjectHandle::invokeCastInt64Method, this, std::placeholders::_1) },
        { L"uint8",
            std::bind(&JuliaObjectHandle::invokeCastUInt8Method, this, std::placeholders::_1) },
        { L"uint16",
            std::bind(&JuliaObjectHandle::invokeCastUInt16Method, this, std::placeholders::_1) },
        { L"uint32",
            std::bind(&JuliaObjectHandle::invokeCastUInt32Method, this, std::placeholders::_1) },
        { L"uint64",
            std::bind(&JuliaObjectHandle::invokeCastUInt64Method, this, std::placeholders::_1) } };
}
//=============================================================================
JuliaObjectHandle::~JuliaObjectHandle() { }
//=============================================================================
void
JuliaObjectHandle::display(Interface* io)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    std::wstring msg = L"  [Julia Object]:";

    bool fails = false;
    std::string asString = jl_value_tRepresentation(
        jlObject, fails, io->getTerminalWidth(), io->getTerminalHeight());

    std::wstring rep;
    if (!fails) {
        rep = utf8_to_wstring(asString);
    }
    msg.append(L"\n");
    msg.append(L"\n");
    io->outputMessage(msg);
    io->outputMessage(std::wstring(L"    ") + rep + L"\n");
}
//=============================================================================
std::string
JuliaObjectHandle::getClassName()
{
    return NLS_HANDLE_JULIA_CATEGORY_STR;
}
//=============================================================================
wstringVector
JuliaObjectHandle::getMethods()
{
    wstringVector methods = getCastMethods();
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return methods;
    }
    stringVector _methods = jl_value_tgetMethodNames(jlObject);
    for (auto& method : _methods) {
        methods.push_back(utf8_to_wstring(method));
    }
    return methods;
}
//=============================================================================
wstringVector
JuliaObjectHandle::getProperties()
{
    wstringVector propertiesList = { L"typeof" };
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return propertiesList;
    }
    stringVector _properties = jl_value_tgetPropertyNames(jlObject);
    for (auto& name : _properties) {
        propertiesList.push_back(utf8_to_wstring(name));
    }

    return propertiesList;
}
//=============================================================================
bool
JuliaObjectHandle::get(const std::wstring& propertyName, ArrayOf& result)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return false;
    }
    if (propertyName == L"typeof") {
        result = ArrayOf::characterArrayConstructor(jl_get_type_of_as_string(jlObject));
        return true;
    }
    if (isProperty(propertyName)) {
        bool wasFound = false;
        result = jl_value_tGetProperty(jlObject, wstring_to_utf8(propertyName), wasFound);
        return wasFound;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::isProperty(const std::wstring& propertyName)
{
    if (propertyName == L"typeof") {
        return true;
    }
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (jlObject) {
        stringVector _names = jl_value_tgetPropertyNames(jlObject);
        auto it = std::find(_names.begin(), _names.end(), wstring_to_utf8(propertyName));
        if (it != _names.end()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::isMethod(const std::wstring& methodName)
{
    if (isCastMethod(methodName)) {
        return true;
    }
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (jlObject) {
        stringVector _methods = jl_value_tgetMethodNames(jlObject);
        auto it = std::find(_methods.begin(), _methods.end(), wstring_to_utf8(methodName));
        if (it != _methods.end()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::isCastMethod(const std::wstring& methodName)
{
    wstringVector methodCastNames = getCastMethods();
    auto it = std::find(methodCastNames.begin(), methodCastNames.end(), methodName);
    return (it != methodCastNames.end());
}
//=============================================================================
wstringVector
JuliaObjectHandle::getCastMethods()
{
    if (!methodCastNames.empty()) {
        return methodCastNames;
    }
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return methodCastNames;
    }
    std::string typeOf = jl_get_type_of_as_string(jlObject);

    wstringVector numericTypes = { L"numeric", L"double", L"single", L"logical", L"int8", L"uint8",
        L"int16", L"uint16", L"int32", L"uint32", L"int64", L"uint64" };

    static const std::unordered_map<std::string, std::string> TYPE_MAPPING
        = { { "Array{ComplexF32", "Matrix{ComplexF32}" },
              { "Array{ComplexF64", "Matrix{ComplexF64}" }, { "Array{Float64", "Matrix{Float64}" },
              { "Array{Float32", "Matrix{Float32}" }, { "Array{Int8", "Matrix{Int8}" },
              { "Array{Int16", "Matrix{Int16}" }, { "Array{Int32", "Matrix{Int32}" },
              { "Array{Int64", "Matrix{Int64}" }, { "Array{UInt8", "Matrix{UInt8}" },
              { "Array{UInt16", "Matrix{UInt16}" }, { "Array{UInt32", "Matrix{UInt32}" },
              { "Array{UInt64", "Matrix{UInt64}" }, { "Vector{Int8}", "Matrix{Int8}" },
              { "Vector{Int16}", "Matrix{Int16}" }, { "Vector{Int32}", "Matrix{Int32}" },
              { "Vector{Int64}", "Matrix{Int64}" }, { "Vector{UInt8}", "Matrix{UInt8}" },
              { "Vector{Float32}", "Matrix{Float32}" }, { "Vector{Float64}", "Matrix{Float64}" },
              { "Vector{UInt16}", "Matrix{UInt16}" }, { "Vector{UInt32}", "Matrix{UInt32}" },
              { "Vector{UInt64}", "Matrix{UInt64}" }, { "Vector{String}", "Matrix{String}" },
              { "Set{", "Set{Any}" }, { "Tuple{", "Tuple{Any}" } };

    for (const auto& [prefix, matrixType] : TYPE_MAPPING) {
        if (StringHelpers::starts_with(typeOf, prefix)) {
            typeOf = matrixType;
            break;
        }
    }

    std::unordered_map<std::string, wstringVector> JL_TYPE_CONVERTERS = {
        { "Matrix{Any}", { L"cell" } },
        { "Matrix{String}", { L"string" } },
        { "Set{Any}", { L"cell" } },
        { "Tuple{Any}", { L"cell" } },
        { "SparseMatrixCSC{ComplexF64, Int64}", { L"numeric", L"double" } },
        { "SparseMatrixCSC{Float64, Int64}", { L"numeric", L"double", L"single" } },
        { "SparseMatrixCSC{Float32, Int64}", { L"numeric", L"double", L"single" } },
        { "SparseMatrixCSC{Bool, Int64}", { L"numeric", L"double", L"single", L"logical" } },
        { "Matrix{ComplexF32}", { L"numeric", L"double", L"single" } },
        { "Matrix{ComplexF64}", { L"numeric", L"double", L"single" } },
        { "Matrix{Int8}", numericTypes },
        { "Matrix{Int16}", numericTypes },
        { "Matrix{Int32}", numericTypes },
        { "Matrix{Int64}", numericTypes },
        { "Matrix{UInt8}", numericTypes },
        { "Matrix{UInt16}", numericTypes },
        { "Matrix{UInt32}", numericTypes },
        { "Matrix{UInt64}", numericTypes },
        { "Matrix{Float64}", numericTypes },
        { "Matrix{Float32}", numericTypes },
    };

    auto converter = JL_TYPE_CONVERTERS.find(typeOf);
    if (converter != JL_TYPE_CONVERTERS.end()) {
        methodCastNames = converter->second;
    }

    return methodCastNames;
}
//=============================================================================
bool
JuliaObjectHandle::invokeMethod(Interface* io, const ArrayOfVector& argIn, int nLhs,
    const std::string& methodName, ArrayOfVector& results)
{
    ArrayOfVector params = argIn;
    params.pop_front();
    return invoke(io, utf8_to_wstring(methodName), params, nLhs, results);
}
//=============================================================================
bool
JuliaObjectHandle::invoke(Interface* io, const std::wstring& methodName,
    const ArrayOfVector& inputs, int nLhs, ArrayOfVector& results)
{
    if (invokeCastMethod(methodName, results)) {
        return true;
    }
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return false;
    }

    NLSjl_eval_string("clear_stdout()");
    NLSjl_eval_string("clear_stderr()");

    NLSjl_exception_clear();

    std::string utf8MethodName = Nelson::wstring_to_utf8(methodName);

    jl_function_t* func = nullptr;
    if (isModule(jlObject)) {
        func = NLSjl_get_function((jl_module_t*)jlObject, utf8MethodName.c_str());
    } else {
        func = (jl_function_t*)NLSjl_get_field(jlObject, utf8MethodName.c_str());
    }

    if (func) {
        jl_value_t** args = (jl_value_t**)malloc(sizeof(jl_value_t*) * inputs.size());
        for (int i = 0; i < inputs.size(); i++) {
            args[i] = (jl_value_t*)ArrayOfTojl_value_t(inputs[i]);
        }
        jl_value_t* ret = NLSjl_call(func, args, inputs.size());
        free(args);
        if (!ret) {
            jl_value_t* exception = NLSjl_exception_occurred();
            if (exception) {
                jl_module_t* jl_base_module = (jl_module_t*)NLSjl_eval_string("Base");
                jl_function_t* sprint_func = NLSjl_get_function(jl_base_module, "sprint");
                jl_function_t* showerror_func = NLSjl_get_function(jl_base_module, "showerror");

                jl_value_t* error_string = NLSjl_call2(sprint_func, showerror_func, exception);
                std::string error_msg = NLSjl_string_ptr(error_string);

                jl_value_t* err = NLSjl_eval_string("get_stderr()");
                if (err) {
                    std::string txt = std::string(NLSjl_string_ptr(err));
                    if (!txt.empty()) {
                        error_msg += "\n" + txt;
                    }
                }
                error_msg = _("Error in Julia: ") + "\n" + error_msg;
                Error(error_msg);
                return {};
            }
        }

        jl_value_t* output = NLSjl_eval_string("get_stdout()");
        if (output) {
            std::string txt = std::string(NLSjl_string_ptr(output));
            if (io) {
                io->outputMessage(utf8_to_wstring(txt));
            }
        }

        if (ret) {
            results.push_back(jl_value_tToArrayOf(ret));
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastMethod(const std::wstring& methodName, ArrayOfVector& results)
{
    auto it = methodMap.find(methodName);
    if (it != methodMap.end()) {
        return it->second(results);
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastCellMethod(ArrayOfVector& results)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return false;
    }
    bool wasConverted;
    results << convertCellToArrayOf(jlObject, wasConverted);
    return wasConverted;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastStringMethod(ArrayOfVector& results)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return false;
    }
    bool wasConverted;
    results << convertMatrixStringToArrayOf(jlObject, wasConverted);
    return wasConverted;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastNumericMethod(ArrayOfVector& results)
{
    jl_value_t* jlObject = (jl_value_t*)this->getPointer();
    if (!jlObject) {
        return false;
    }
    std::string typeOf = jl_get_type_of_as_string(jlObject);
    static const std::unordered_map<std::string, std::string> TYPE_MAPPING = {
        { "Array{ComplexF32", "Matrix{ComplexF32}" },
        { "Array{ComplexF64", "Matrix{ComplexF64}" },
        { "Array{Float64", "Matrix{Float64}" },
        { "Array{Float32", "Matrix{Float32}" },
        { "Array{Int8", "Matrix{Int8}" },
        { "Array{Int16", "Matrix{Int16}" },
        { "Array{Int32", "Matrix{Int32}" },
        { "Array{Int64", "Matrix{Int64}" },
        { "Array{UInt8", "Matrix{UInt8}" },
        { "Array{UInt16", "Matrix{UInt16}" },
        { "Array{UInt32", "Matrix{UInt32}" },
        { "Array{UInt64", "Matrix{UInt64}" },
    };

    for (const auto& [prefix, matrixType] : TYPE_MAPPING) {
        if (StringHelpers::starts_with(typeOf, prefix)) {
            typeOf = matrixType;
            break;
        }
    }

    std::unordered_map<std::string, ArrayOf (*)(jl_value_t * value, bool& wasConverted)>
        TYPES_FUNCTIONS_CONVERTERS = {
            { "SparseMatrixCSC{ComplexF64, Int64}", convertSparseMatrixComplexFloat64ToArrayOf },
            { "SparseMatrixCSC{Float64, Int64}", convertSparseMatrixFloat64ToArrayOf },
            { "SparseMatrixCSC{Float32, Int64}", convertSparseMatrixFloat32ToArrayOf },
            { "SparseMatrixCSC{Bool, Int64}", convertSparseMatrixBoolToArrayOf },
            { "Matrix{ComplexF32}", convertMatrixComplexFloat32ToArrayOf },
            { "Matrix{ComplexF64}", convertMatrixComplexFloat64ToArrayOf },
            { "Matrix{Float64}", convertMatrixFloat64ToArrayOf },
            { "Matrix{Float32}", convertMatrixFloat32ToArrayOf },
            { "Vector{Float32}", convertMatrixFloat32ToArrayOf },
            { "Vector{Float64}", convertMatrixFloat64ToArrayOf },
            { "Vector{UInt8}", convertMatrixUInt8ToArrayOf },
            { "Vector{UInt16}", convertMatrixUInt16ToArrayOf },
            { "Vector{UInt32}", convertMatrixUInt32ToArrayOf },
            { "Vector{UInt64}", convertMatrixUInt64ToArrayOf },
            { "Vector{Int8}", convertMatrixInt8ToArrayOf },
            { "Vector{Int16}", convertMatrixInt16ToArrayOf },
            { "Vector{Int32}", convertMatrixInt32ToArrayOf },
            { "Vector{Int64}", convertMatrixInt64ToArrayOf },
            { "Matrix{UInt8}", convertMatrixUInt8ToArrayOf },
            { "Matrix{UInt16}", convertMatrixUInt16ToArrayOf },
            { "Matrix{UInt32}", convertMatrixUInt32ToArrayOf },
            { "Matrix{UInt64}", convertMatrixUInt64ToArrayOf },
            { "Matrix{Int8}", convertMatrixInt8ToArrayOf },
            { "Matrix{Int16}", convertMatrixInt16ToArrayOf },
            { "Matrix{Int32}", convertMatrixInt32ToArrayOf },
            { "Matrix{Int64}", convertMatrixInt64ToArrayOf },
            { "Matrix{String}", convertMatrixStringToArrayOf },
        };
    auto converter = TYPES_FUNCTIONS_CONVERTERS.find(typeOf);
    bool wasConverted = false;
    if (converter != TYPES_FUNCTIONS_CONVERTERS.end()) {
        results << converter->second(jlObject, wasConverted);
    }
    return wasConverted;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastDoubleMethod(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1 && !results[0].isDoubleClass()) {
        if (results[0].isComplex()) {
            results[0].promoteType(NLS_DCOMPLEX);
        } else {
            results[0].promoteType(NLS_DOUBLE);
        }
        return true;
    }
    if (converted && results.size() == 1 && results[0].isDoubleClass()) {
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastSingleMethod(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1 && !results[0].isSingleClass()) {
        if (results[0].isComplex()) {
            results[0].promoteType(NLS_SCOMPLEX);
        } else {
            results[0].promoteType(NLS_SINGLE);
        }
        return true;
    }
    if (converted && results.size() == 1 && results[0].isDoubleClass()) {
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastInt8Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_INT8);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastInt16Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_INT16);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastInt32Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_INT32);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastInt64Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_INT64);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastUInt8Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_UINT8);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastUInt16Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_UINT16);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastUInt32Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_UINT32);
        return true;
    }
    return false;
}
//=============================================================================
bool
JuliaObjectHandle::invokeCastUInt64Method(ArrayOfVector& results)
{
    bool converted = invokeCastNumericMethod(results);
    if (converted && results.size() == 1) {
        results[0].promoteType(NLS_UINT64);
        return true;
    }
    return false;
}
//=============================================================================
}
//=============================================================================
