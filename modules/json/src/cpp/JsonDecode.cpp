//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include <simdjson.h>
#include "StringHelpers.hpp"
#include <vector>
#include <cstdlib>
#include "MakeValidFieldname.hpp"
#include "JsonDecode.hpp"
#include "characters_encoding.hpp"
#include "JsonVariable.hpp"
#include "nlsBuildConfig.h"
#include "i18n.hpp"
#include "ParallelTransform.hpp"
#include "omp_for_loop.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using namespace simdjson;
//=============================================================================
static double
returnInfinity(bool bPositive)
{
    double res = std::numeric_limits<double>::infinity();
    if (!bPositive) {
        res = -res;
    }
    return res;
}
//=============================================================================
static JSON_TO_NELSON_Type
findCommonJsonVariableType(JsonVariable& jsVar)
{
    if (jsVar.vectorJsonVariable.empty()) {
        return JSON_TO_NELSON_UNDEFINED;
    }

    // Use a bitmap approach for faster type counting
    size_t typeCount[5] = { 0, 0, 0, 0, 0 }; // DOUBLE, LOGICAL, EMPTY, STRUCT, OTHER

    for (const auto& element : jsVar.vectorJsonVariable) {
        switch (element->jsonVariableType) {
        case JSON_TO_NELSON_DOUBLE:
            ++typeCount[0];
            break;
        case JSON_TO_NELSON_LOGICAL:
            ++typeCount[1];
            break;
        case JSON_TO_NELSON_EMPTY_MATRIX:
            ++typeCount[2];
            break;
        case JSON_TO_NELSON_STRUCT:
            ++typeCount[3];
            break;
        default:
            // Early termination - if we find any OTHER type, return UNDEFINED immediately
            return JSON_TO_NELSON_UNDEFINED;
        }
    }

    const size_t total = jsVar.vectorJsonVariable.size();

    // Now check for homogeneous types in order of most restrictive to least
    if (typeCount[1] == total) {
        return JSON_TO_NELSON_LOGICAL;
    }
    if (typeCount[3] == total) {
        return JSON_TO_NELSON_STRUCT;
    }
    if (typeCount[0] + typeCount[2] == total) {
        return (typeCount[0] > 0) ? JSON_TO_NELSON_DOUBLE : JSON_TO_NELSON_EMPTY_MATRIX;
    }

    // If we got here, it's a mixed type
    return JSON_TO_NELSON_UNDEFINED;
}
//=============================================================================
static double
parseSpecialNumeric(std::string_view token)
{
    if (token == "NaN") {
        return std::numeric_limits<double>::quiet_NaN();
    }
    if (token == "Inf") {
        return std::numeric_limits<double>::infinity();
    }
    if (token == "-Inf") {
        return -std::numeric_limits<double>::infinity();
    }
    // If not a special value, return NaN as fallback
    return std::numeric_limits<double>::quiet_NaN();
}
//=============================================================================
static bool
convertToJsonVariable(ondemand::value& value, JsonVariable& jsVar)
{
    // Try boolean first (most decisive and fastest to check)
    {
        auto bool_result = value.get_bool();
        if (!bool_result.error()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_LOGICAL;
            jsVar.scalarLogical = bool_result.value();
            return true;
        }
    }

    // Try number (double, int64, uint64)
    {
        // Try double first since it's most common
        auto double_result = value.get_double();
        if (!double_result.error()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
            jsVar.scalarDouble = double_result.value();
            return true;
        }

        // Check for special numeric values when double parsing fails
        if (double_result.error() == simdjson::INCORRECT_TYPE) {
            try {
                std::string_view token = value.raw_json_token();
                if (token == "null") {
                    jsVar.jsonVariableType = JSON_TO_NELSON_EMPTY_MATRIX;
                    jsVar.scalarDouble = std::nan("");
                    return true;
                }

                // Fast check for special values
                if (token == "NaN" || token == "Inf" || token == "+Inf" || token == "-Inf") {
                    jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
                    jsVar.scalarDouble = parseSpecialNumeric(token);
                    return true;
                }
            } catch (...) {
                // Continue to other type checks
            }
        }
    }

    // Try integers - combine int64/uint64 checks
    {
        auto int64_result = value.get_int64();
        if (!int64_result.error()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
            jsVar.scalarDouble = static_cast<double>(int64_result.value());
            return true;
        }

        auto uint64_result = value.get_uint64();
        if (!uint64_result.error()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
            jsVar.scalarDouble = static_cast<double>(uint64_result.value());
            return true;
        }
    }

    // Try string last as it's typically more expensive
    {
        auto string_result = value.get_string();
        if (!string_result.error()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_STRING;
            jsVar.scalarString = string_result.value();
            return true;
        }
    }

    return false;
}
//=============================================================================
static bool
transformStringArray(JsonVariable& jsVar, size_t totaldims)
{
    switch (jsVar.dims.size()) {
    case 1: {
        jsVar.vectorString.resize(totaldims * jsVar.vectorJsonVariable.size());
        parallelTransform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
            jsVar.vectorString.begin(), [](JsonVariable* val) { return val->scalarString; });
    } break;
    case 2: {
        jsVar.vectorString.resize(totaldims * jsVar.vectorJsonVariable.size());
        size_t rows = jsVar.dims[0];
        size_t cols = jsVar.dims[1];
        for (size_t idx = 0; idx < rows * cols; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            std::string val = jsVar.vectorJsonVariable[i]->vectorString[j];
            jsVar.vectorString[j * rows + i] = val;
        }
    } break;
    default: {
        std::vector<std::string> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element->vectorString.begin(), element->vectorString.end());
        }
        size_t lastdimlen = jsVar.dims[jsVar.dims.size() - 1];
        size_t elementCount = 1;
        for (size_t dim : jsVar.dims) {
            elementCount *= dim;
        }
        size_t ymax = elementCount / lastdimlen;
        jsVar.vectorString.reserve(elementCount);
        for (size_t idx = 0; idx < elementCount; ++idx) {
            size_t i = idx % ymax;
            size_t j = idx / ymax;
            jsVar.vectorString.insert(jsVar.vectorString.end(), vectTemp[j * ymax + i]);
        }
    } break;
    }
    jsVar.jsonVariableType = JSON_TO_NELSON_STRING;
    jsVar.vectorJsonVariable.clear();
    jsVar.reduced = true;
    return true;
}
//=============================================================================
static bool
transformLogicalArray(JsonVariable& jsVar, size_t totaldims)
{
    switch (jsVar.dims.size()) {
    case 1: {
        jsVar.vectorLogical.resize(totaldims * jsVar.vectorJsonVariable.size());
        parallelTransform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
            jsVar.vectorLogical.begin(), [](JsonVariable* val) { return val->scalarLogical; });
    } break;
    case 2: {
        jsVar.vectorLogical.resize(totaldims * jsVar.vectorJsonVariable.size());
        size_t rows = jsVar.dims[0];
        size_t cols = jsVar.dims[1];
        size_t total = rows * cols;
        for (size_t idx = 0; idx < total; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            logical val = jsVar.vectorJsonVariable[i]->vectorLogical[j];
            jsVar.vectorLogical[j * rows + i] = val;
        }
    } break;
    default: {
        std::vector<logical> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element->vectorLogical.begin(), element->vectorLogical.end());
        }
        size_t lastdimlen = jsVar.dims[jsVar.dims.size() - 1];
        size_t elementCount = 1;
        for (size_t dim : jsVar.dims) {
            elementCount *= dim;
        }
        size_t ymax = elementCount / lastdimlen;
        jsVar.vectorLogical.reserve(elementCount);
        for (size_t i = 0; i < ymax; ++i) {
            for (size_t j = 0; j < lastdimlen; ++j) {
                jsVar.vectorLogical.insert(jsVar.vectorLogical.end(), vectTemp[j * ymax + i]);
            }
        }
    } break;
    }
    jsVar.jsonVariableType = JSON_TO_NELSON_LOGICAL;
    jsVar.vectorJsonVariable.clear();
    jsVar.reduced = true;
    return true;
}
//=============================================================================
static bool
transformDoubleArray(JsonVariable& jsVar, size_t totaldims)
{
    const size_t elementCount = jsVar.vectorJsonVariable.size();

    switch (jsVar.dims.size()) {
    case 1: {
        // Pre-allocate all memory needed
        jsVar.vectorDouble.resize(elementCount);

        // Use parallel transform for large arrays
        if (elementCount > 1000) {
            parallelTransform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
                jsVar.vectorDouble.begin(), [](JsonVariable* val) { return val->scalarDouble; });
        } else {
            std::transform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
                jsVar.vectorDouble.begin(), [](JsonVariable* val) { return val->scalarDouble; });
        }
    } break;
    case 2: {
        const size_t rows = jsVar.dims[0];
        const size_t cols = jsVar.dims[1];
        const size_t total = rows * cols;

        // Pre-allocate all memory needed
        jsVar.vectorDouble.resize(total);

        // For small matrices, use a single-pass transform with computed indices
        OMP_PARALLEL_FOR_LOOP(total)
        for (ompIndexType idx = 0; idx < (ompIndexType)total; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            jsVar.vectorDouble[j * rows + i] = jsVar.vectorJsonVariable[i]->vectorDouble[j];
        }
    } break;
    default: {
        std::vector<double> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element->vectorDouble.begin(), element->vectorDouble.end());
        }
        size_t lastdimlen = jsVar.dims[jsVar.dims.size() - 1];
        size_t totalElementCount = 1;
        for (size_t dim : jsVar.dims) {
            totalElementCount *= dim;
        }
        size_t ymax = totalElementCount / lastdimlen;
        size_t k = 0;
        jsVar.vectorDouble.reserve(totalElementCount);
        for (size_t i = 0; i < ymax; ++i) {
            for (size_t j = 0; j < lastdimlen; ++j) {
                jsVar.vectorDouble.insert(jsVar.vectorDouble.end(), vectTemp[j * ymax + i]);
            }
        }
    } break;
    }

    jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
    jsVar.vectorJsonVariable.clear();
    jsVar.reduced = true;
    return true;
}
//=============================================================================
static bool
transformStructArray(JsonVariable& jsVar, size_t totaldims)
{
    std::vector<std::string> fieldnamesRef;
    for (const auto& elements : jsVar.vectorJsonVariable[0]->fieldnames) {
        fieldnamesRef.push_back(elements);
    }
    for (size_t i = 1; i < jsVar.vectorJsonVariable.size(); ++i) {
        if (jsVar.vectorJsonVariable[i]->fieldnames.size() != fieldnamesRef.size()) {
            jsVar.reduced = true;
            jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
            return true;
        }
        for (const auto& element : jsVar.vectorJsonVariable[i]->fieldnames) {
            if (std::find(fieldnamesRef.begin(), fieldnamesRef.end(), element)
                == fieldnamesRef.end()) {
                jsVar.reduced = true;
                jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
                return true;
            }
        }
    }
    jsVar.vectorJsonVariable.reserve(fieldnamesRef.size());
    switch (jsVar.dims.size()) {
    case 1: {
        for (const auto& name : fieldnamesRef) {
            for (auto var : jsVar.vectorJsonVariable) {
                jsVar.map[name].push_back(var->scalarMap[name]);
            }
        }
    } break;
    case 2: {
        for (const auto& name : fieldnamesRef) {
            size_t rows = jsVar.dims[0];
            size_t cols = jsVar.dims[1];
            jsVar.map[name].reserve(rows * cols);
            for (size_t idx = 0; idx < rows * cols; ++idx) {
                size_t i = idx % rows;
                size_t j = idx / rows;
                auto& var = jsVar.vectorJsonVariable[i];
                if (var->dims.size() != 0) {
                    jsVar.map[name].push_back(var->map[name][j]);
                } else {
                    jsVar.map[name].push_back(var->scalarMap[name]);
                }
            }
        }
    } break;
    default: {
        for (const auto& name : fieldnamesRef) {
            jsVar.map[name].reserve(jsVar.vectorJsonVariable.size());
            for (auto var : jsVar.vectorJsonVariable) {
                if (var->dims.size() != 0) {
                    jsVar.map[name].insert(
                        jsVar.map[name].end(), var->map[name].begin(), var->map[name].end());
                } else {
                    jsVar.map[name].push_back(var->vectorJsonVariable[0]->scalarMap[name]);
                }
            }
        }
    } break;
    }
    jsVar.fieldnames = fieldnamesRef;
    jsVar.jsonVariableType = JSON_TO_NELSON_STRUCT;
    jsVar.vectorJsonVariable.clear();
    jsVar.reduced = true;
    return true;
}
//=============================================================================
static bool
importElement(ondemand::value& value, JsonVariable& jsVar)
{
    // Try array first since it needs special processing
    {
        auto array_result = value.get_array();
        if (!array_result.error()) {
            auto arr = array_result.value();
            jsVar.jsonVariableType = JSON_TO_NELSON_ARRAY;

            // First pass: count elements to pre-allocate
            size_t count = arr.count_elements();

            // Reserve space to avoid reallocations
            jsVar.vectorJsonVariable.reserve(count);

            // Process all array elements
            for (auto element : arr) {
                // IMPORTANT FIX: Extract the value from simdjson_result
                auto elem_value_result = element.value();
                if (element.error()) {
                    return false;
                }
                auto elem_value = element.value();

                // Create JsonVariable on heap only once
                auto subVar = new JsonVariable();
                if (importElement(elem_value, *subVar)) {
                    jsVar.vectorJsonVariable.push_back(subVar);
                } else {
                    delete subVar;
                    return false;
                }
            }

            // Rest of the function remains the same...
            JSON_TO_NELSON_Type commonType = findCommonJsonVariableType(jsVar);
            // ... existing code ...
            if (commonType != JSON_TO_NELSON_UNDEFINED) {
                // Check dimensions - if we have any elements
                if (!jsVar.vectorJsonVariable.empty()) {
                    std::vector<size_t> refVar = jsVar.vectorJsonVariable[0]->dims;
                    size_t totaldims = 1;
                    for (auto i : refVar) {
                        totaldims *= i;
                    }

                    // Fast dimension checking with early exit
                    bool dimensionsMatch = true;
                    for (const auto& element : jsVar.vectorJsonVariable) {
                        const std::vector<size_t>& dims = element->dims;
                        if (refVar.size() != dims.size()) {
                            dimensionsMatch = false;
                            break;
                        }

                        for (size_t j = 0; j < refVar.size(); ++j) {
                            if (refVar[j] != dims[j]) {
                                dimensionsMatch = false;
                                break;
                            }
                        }

                        if (!dimensionsMatch) {
                            break;
                        }
                    }

                    if (!dimensionsMatch) {
                        jsVar.reduced = true;
                        jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
                        return true;
                    }

                    // Set dimensions efficiently
                    if (refVar.empty()) {
                        jsVar.dims.push_back(jsVar.vectorJsonVariable.size());
                    } else if (refVar.size() == 1) {
                        jsVar.dims.resize(2);
                        jsVar.dims[0] = jsVar.vectorJsonVariable.size();
                        jsVar.dims[1] = refVar[0];
                    } else {
                        jsVar.dims = refVar;
                        jsVar.dims.push_back(jsVar.vectorJsonVariable.size());
                    }

                    // Transform based on common type with optimized functions
                    switch (commonType) {
                    case JSON_TO_NELSON_EMPTY_MATRIX:
                        return true;
                    case JSON_TO_NELSON_DOUBLE:
                        return transformDoubleArray(jsVar, totaldims);
                    case JSON_TO_NELSON_LOGICAL:
                        return transformLogicalArray(jsVar, totaldims);
                    case JSON_TO_NELSON_STRING:
                        return transformStringArray(jsVar, totaldims);
                    case JSON_TO_NELSON_STRUCT:
                        return transformStructArray(jsVar, totaldims);
                    default:
                        break;
                    }
                }
            } else {
                jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
            }
            return true;
        }
    }

    // Try object with optimized field handling
    {
        auto object_result = value.get_object();
        if (!object_result.error()) {
            auto obj = object_result.value();
            jsVar.jsonVariableType = JSON_TO_NELSON_STRUCT;

            // Pre-count fields to reserve memory
            size_t field_count = obj.count_fields();
            jsVar.fieldnames.reserve(field_count);

            for (auto field : obj) {
                auto key_result = field.unescaped_key();
                if (key_result.error()) {
                    continue;
                }

                std::string key = MakeValidFieldname(std::string(key_result.value()));
                auto valVar = new JsonVariable();

                // IMPORTANT FIX: Extract the value from simdjson_result
                auto field_value_result = field.value();
                if (field_value_result.error()) {
                    delete valVar;
                    continue;
                }
                auto field_value = field_value_result.value();

                if (importElement(field_value, *valVar)) {
                    auto it = jsVar.scalarMap.find(key);
                    if (it == jsVar.scalarMap.end()) {
                        jsVar.scalarMap[key] = valVar;
                        jsVar.fieldnames.push_back(std::move(key));
                    } else {
                        size_t idx = 1;
                        std::string modifiedKey;
                        bool found = false;
                        do {
                            modifiedKey = key + "_" + std::to_string(idx);
                            auto fieldnameIt = std::find(
                                jsVar.fieldnames.begin(), jsVar.fieldnames.end(), modifiedKey);
                            found = (fieldnameIt != jsVar.fieldnames.end());
                            idx++;
                        } while (found);

                        jsVar.scalarMap[modifiedKey] = valVar;
                        jsVar.fieldnames.push_back(std::move(modifiedKey));
                    }
                } else {
                    delete valVar;
                }
            }
            return true;
        }
    }

    // If not array or object, try scalar values
    return convertToJsonVariable(value, jsVar);
}
//=============================================================================
static ArrayOf
jsonVariableToNelson(JsonVariable& jsVar);
//=============================================================================
static ArrayOf
jsonVariableToNelsonStringType(JsonVariable& jsVar)
{
    switch (jsVar.dims.size()) {
    case 0: {
        return ArrayOf::characterArrayConstructor(std::string(jsVar.scalarString));
    } break;
    case 1: {
        Dimensions dims(jsVar.dims[0], 1);
        ArrayOf* dptr = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
        for (size_t k = 0; k < jsVar.vectorString.size(); k++) {
            dptr[k] = ArrayOf::characterArrayConstructor(jsVar.vectorString[k]);
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, dptr);
    } break;
    case 2: {
        Dimensions dims(jsVar.dims[0], jsVar.dims[1]);
        ArrayOf* dptr = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
        for (size_t k = 0; k < jsVar.vectorString.size(); k++) {
            dptr[k] = ArrayOf::characterArrayConstructor(jsVar.vectorString[k]);
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, dptr);
    } break;
    default: {
        Dimensions dims;
        for (size_t i = 0; i < jsVar.dims.size(); ++i) {
            dims[i] = jsVar.dims[i];
        }
        ArrayOf* dptr = (ArrayOf*)ArrayOf::allocateArrayOf(
            NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
        for (size_t k = 0; k < jsVar.vectorString.size(); k++) {
            dptr[k] = ArrayOf::characterArrayConstructor(jsVar.vectorString[k]);
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, dptr);
    } break;
    }
    ArrayOf res = ArrayOf::emptyConstructor();
    res.promoteType(NLS_CHAR);
    return res;
}
//=============================================================================
static ArrayOf
jsonVariableToNelsonLogicalType(JsonVariable& jsVar)
{
    switch (jsVar.dims.size()) {
    case 0: {
        return ArrayOf::logicalConstructor(jsVar.scalarLogical);
    } break;
    case 1: {
        Dimensions dims(jsVar.dims[0], 1);
        logical* dptr = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorLogical.data(), sizeof(bool) * jsVar.vectorLogical.size());
        return ArrayOf(NLS_LOGICAL, dims, dptr);
    } break;
    case 2: {
        Dimensions dims(jsVar.dims[0], jsVar.dims[1]);
        logical* dptr = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorLogical.data(), sizeof(logical) * jsVar.vectorLogical.size());
        return ArrayOf(NLS_LOGICAL, dims, dptr);
    } break;
    default: {
        Dimensions dims;
        for (size_t i = 0; i < jsVar.dims.size(); ++i) {
            dims[i] = jsVar.dims[i];
        }
        logical* dptr = (logical*)ArrayOf::allocateArrayOf(
            NLS_LOGICAL, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorLogical.data(), sizeof(logical) * jsVar.vectorLogical.size());
        return ArrayOf(NLS_LOGICAL, dims, dptr);
    } break;
    }
    ArrayOf res = ArrayOf::emptyConstructor();
    res.promoteType(NLS_LOGICAL);
    return res;
}
//=============================================================================
static ArrayOf
jsonVariableToNelsonDoubleType(JsonVariable& jsVar)
{
    switch (jsVar.dims.size()) {
    case 0: {
        return ArrayOf::doubleConstructor(jsVar.scalarDouble);
    } break;
    case 1: {
        Dimensions dims(jsVar.dims[0], 1);
        double* dptr = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorDouble.data(), sizeof(double) * jsVar.vectorDouble.size());
        return ArrayOf(NLS_DOUBLE, dims, dptr);
    } break;
    case 2: {
        Dimensions dims(jsVar.dims[0], jsVar.dims[1]);
        double* dptr = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorDouble.data(), sizeof(double) * jsVar.vectorDouble.size());
        return ArrayOf(NLS_DOUBLE, dims, dptr);
    } break;
    default: {
        Dimensions dims;
        for (size_t i = 0; i < jsVar.dims.size(); ++i) {
            dims[i] = jsVar.dims[i];
        }
        double* dptr = (double*)ArrayOf::allocateArrayOf(
            NLS_DOUBLE, dims.getElementCount(), stringVector(), false);
        memcpy(dptr, jsVar.vectorDouble.data(), sizeof(double) * jsVar.vectorDouble.size());
        return ArrayOf(NLS_DOUBLE, dims, dptr);
    } break;
    }
    return ArrayOf::emptyConstructor();
}
//=============================================================================
static ArrayOf
jsonVariableToNelsonStructType(JsonVariable& jsVar, Dimensions& dims)
{
    stringVector fieldnames = jsVar.fieldnames;
    ArrayOf* ptrStruct = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_STRUCT_ARRAY, dims.getElementCount(), fieldnames, false);
    size_t offset = 0;
    indexType elementCount = dims.getElementCount();
    for (indexType j = 0; j < elementCount; j++) {
        size_t s = jsVar.fieldnames.size();
        for (size_t i = 0; i < s; i++) {
            ArrayOf rval = jsonVariableToNelson(*jsVar.map.at(jsVar.fieldnames[i])[j]);
            ptrStruct[offset] = rval;
            offset++;
        }
    }
    return ArrayOf(NLS_STRUCT_ARRAY, dims, ptrStruct, false, fieldnames);
}
//=============================================================================
static ArrayOf
jsonVariableToNelsonStructType(JsonVariable& jsVar)
{
    switch (jsVar.dims.size()) {
    case 0: {
        if (jsVar.fieldnames.size() == 0) {
            return ArrayOf::emptyStructWithoutFields();
        }
        stringVector fieldnames;
        ArrayOfVector fieldvalues;
        for (size_t k = 0; k < jsVar.fieldnames.size(); k++) {
            fieldnames.push_back(jsVar.fieldnames[k]);
            JsonVariable jsv = *jsVar.scalarMap[jsVar.fieldnames[k]];
            ArrayOf fieldvalue = jsonVariableToNelson(jsv);
            fieldvalues.push_back(fieldvalue);
        }
        return ArrayOf::structScalarConstructor(fieldnames, fieldvalues);

    } break;
    case 1: {
        if (jsVar.dims[0] == 1) {
            stringVector fieldnames;
            ArrayOfVector fieldvalues;
            for (size_t k = 0; k < jsVar.fieldnames.size(); k++) {
                fieldnames.push_back(jsVar.fieldnames[k]);
                fieldvalues.push_back(jsonVariableToNelson(*jsVar.map.at(jsVar.fieldnames[k])[0]));
            }
            return ArrayOf::structScalarConstructor(fieldnames, fieldvalues);
        }
        Dimensions dims(jsVar.dims[0], 1);
        return jsonVariableToNelsonStructType(jsVar, dims);
    }
    default: {
        Dimensions dims;
        for (size_t i = 0; i < jsVar.dims.size(); ++i) {
            dims[i] = jsVar.dims[i];
        }
        return jsonVariableToNelsonStructType(jsVar, dims);
    } break;
    }
}
//=============================================================================
static ArrayOf
jsonVariableToNelsonCellType(JsonVariable& jsVar)
{
    Dimensions dims(jsVar.vectorJsonVariable.size(), 1);
    ArrayOf* dptr = (ArrayOf*)ArrayOf::allocateArrayOf(
        NLS_CELL_ARRAY, dims.getElementCount(), stringVector(), false);
    ompIndexType elementCount = dims.getElementCount();
    for (ompIndexType k = 0; k < elementCount; k++) {
        dptr[k] = jsonVariableToNelson(*jsVar.vectorJsonVariable[k]);
    }
    return ArrayOf(NLS_CELL_ARRAY, dims, dptr);
}
//=============================================================================
static ArrayOf
jsonVariableToNelson(JsonVariable& jsVar)
{
    switch (jsVar.jsonVariableType) {
    case JSON_TO_NELSON_LOGICAL: {
        return jsonVariableToNelsonLogicalType(jsVar);
    } break;
    case JSON_TO_NELSON_DOUBLE: {
        return jsonVariableToNelsonDoubleType(jsVar);
    } break;
    case JSON_TO_NELSON_STRING: {
        return jsonVariableToNelsonStringType(jsVar);
    } break;
    case JSON_TO_NELSON_UNDEFINED:
    case JSON_TO_NELSON_ARRAY:
    case JSON_TO_NELSON_CELL: {
        return jsonVariableToNelsonCellType(jsVar);
    } break;
    case JSON_TO_NELSON_EMPTY_MATRIX: {
        return ArrayOf::emptyConstructor();
    } break;
    case JSON_TO_NELSON_STRUCT: {
        return jsonVariableToNelsonStructType(jsVar);
    } break;
    }
    return {};
}
//=============================================================================
static ArrayOf
jsonDecodeInternal(const simdjson::padded_string& json_padded, std::string& errorMessage)
{
    if (json_padded.size() == 0) {
        errorMessage = _("valid JSON Object expected.");
        return {};
    }
    simdjson::ondemand::parser parser;

    simdjson::ondemand::document doc;
    JsonVariable jsVar;

    try {
        doc = parser.iterate(json_padded);
        if (doc.is_scalar()) {
            // Handle different scalar types explicitly if needed
            switch (doc.type()) {
            case simdjson::ondemand::json_type::string: {
                // Handle string
                auto content = doc.get_string();
                jsVar.jsonVariableType = JSON_TO_NELSON_STRING;
                jsVar.scalarString = content.value();
                jsVar.reduced = true;
            } break;
            case simdjson::ondemand::json_type::number: {

                auto double_result = doc.get_double();
                if (!double_result.error()) {
                    double num = double_result.value();
                    // Handle the double value
                    jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
                    jsVar.scalarDouble = num;
                    jsVar.reduced = true;
                } else {
                    std::string_view content = doc.raw_json_token();
                    if (content == "-Inf") {
                        jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
                        jsVar.scalarDouble = returnInfinity(false);
                        jsVar.reduced = true;
                    }
                }
            } break;
            case simdjson::ondemand::json_type::boolean: {
                // Handle boolean
                auto bool_result = doc.get_bool();
                if (!bool_result.error()) {
                    bool val = bool_result.value();
                    // Handle the double value
                    jsVar.jsonVariableType = JSON_TO_NELSON_LOGICAL;
                    jsVar.scalarLogical = val;
                    jsVar.reduced = true;
                } else {
                    errorMessage = _("Invalid JSON scalar value.");
                    return {};
                }
            } break;
            case simdjson::ondemand::json_type::null: {
                // Handle null
                jsVar.jsonVariableType = JSON_TO_NELSON_EMPTY_MATRIX;
                jsVar.reduced = true;

            } break;
            default: {
                if (doc.is_alive()) {
                    std::string_view contentView = doc.raw_json_token();
                    std::string jsonString(contentView);
                    StringHelpers::trim(jsonString);
                    if (jsonString == "NaN" || jsonString == "Inf" || jsonString == "-Inf") {
                        jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
                        jsVar.scalarDouble = parseSpecialNumeric(jsonString);
                        jsVar.reduced = true;
                        return jsonVariableToNelson(jsVar);
                    }
                }
                errorMessage = _("Invalid JSON scalar value.");
                return {};
            } break;
            }
        } else {
            auto root_value_result = doc.get_value();
            if (root_value_result.error()) {
                errorMessage = _("Failed to parse JSON structure.");
                return {};
            }
            auto root_value = root_value_result.value();

            if (!importElement(root_value, jsVar)) {
                errorMessage = _("valid JSON Object expected.");
                return {};
            }
        }
        return jsonVariableToNelson(jsVar);
    } catch (const simdjson::simdjson_error& e) {
        if (doc.is_alive()) {
            std::string_view contentView = doc.raw_json_token();
            std::string jsonString(contentView);
            StringHelpers::trim(jsonString);
            if (jsonString == "NaN" || jsonString == "Inf" || jsonString == "-Inf") {
                jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
                jsVar.scalarDouble = parseSpecialNumeric(jsonString);
                jsVar.reduced = true;
                return jsonVariableToNelson(jsVar);
            }
        }
        if (e.error() == simdjson::TAPE_ERROR) {
            errorMessage
                = _("invalid structure: missing or superfluous commas, braces, missing keys, etc.");
        } else {
            errorMessage = e.what();
        }
        return {};
    } catch (const std::runtime_error&) {
        errorMessage = _("valid JSON Object expected.");
        return {};
    }
}
//=============================================================================
ArrayOf
jsonDecodeFile(const std::wstring& filename, std::string& errorMessage)
{
#ifdef _MSC_BUILD
    std::FILE* fp = _wfopen(filename.c_str(), L"rb");
#else
    std::FILE* fp = fopen(wstring_to_utf8(filename).c_str(), "rb");
#endif

    if (fp == nullptr) {
        errorMessage = _("Impossible to open file.");
        return {};
    }

    // Get the file size
    int ret;
#if SIMDJSON_VISUAL_STUDIO && !SIMDJSON_IS_32BITS
    ret = _fseeki64(fp, 0, SEEK_END);
#else
    ret = std::fseek(fp, 0, SEEK_END);
#endif // _WIN64
    if (ret < 0) {
        std::fclose(fp);
        errorMessage = _("Impossible to open file.");
        return {};
    }
#if SIMDJSON_VISUAL_STUDIO && !SIMDJSON_IS_32BITS
    __int64 llen = _ftelli64(fp);
    if (llen == -1L) {
        std::fclose(fp);
        errorMessage = _("Impossible to open file.");
        return {};
    }
#else
    long llen = std::ftell(fp);
    if ((llen < 0) || (llen == LONG_MAX)) {
        std::fclose(fp);
        errorMessage = _("Impossible to open file.");
        return {};
    }
#endif

    // Allocate the padded_string
    size_t len = static_cast<size_t>(llen);
    padded_string json_padded(len);
    if (json_padded.data() == nullptr) {
        std::fclose(fp);
        errorMessage = _("File too big to load in memory.");
        return {};
    }

    // Read the padded_string
    std::rewind(fp);
    size_t bytes_read = std::fread(json_padded.data(), 1, len, fp);
    if (std::fclose(fp) != 0 || bytes_read != len) {
        errorMessage = _("Impossible to read file.");
        return {};
    }
    return jsonDecodeInternal(json_padded, errorMessage);
}
//=============================================================================
ArrayOf
jsonDecode(const std::string& stringToDecode, std::string& errorMessage)
{
    return jsonDecodeInternal(simdjson::padded_string(stringToDecode), errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
