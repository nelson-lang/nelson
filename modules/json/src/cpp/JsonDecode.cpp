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
#pragma warning(disable : 4477)
#endif
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define JSMN_STRICT
#include <jsmn.h>
#include <fast_float/fast_float.h>
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
//=============================================================================
namespace Nelson {
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
static std::string
decodeCharacters(const std::string& str)
{
    std::string res;
    res.reserve(str.size());
    for (size_t k = 0; k < str.size(); ++k) {
        if (str[k] == '\\' && k + 1 < str.size()) {
            if (str[k + 1] == 'u' && k + 5 <= str.size()) {
                unsigned int codepoint = 0;
                for (int i = 0; i < 4; ++i) {
                    char c = str[k + 2 + i];
                    codepoint <<= 4;
                    if (c >= '0' && c <= '9')
                        codepoint += c - '0';
                    else if (c >= 'A' && c <= 'F')
                        codepoint += c - 'A' + 10;
                    else if (c >= 'a' && c <= 'f')
                        codepoint += c - 'a' + 10;
                    else
                        break;
                }
                if (codepoint < 0x80)
                    res.push_back(static_cast<char>(codepoint));
                else if (codepoint < 0x800) {
                    res.push_back(static_cast<char>(0xC0 | (codepoint >> 6)));
                    res.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
                } else {
                    res.push_back(static_cast<char>(0xE0 | (codepoint >> 12)));
                    res.push_back(static_cast<char>(0x80 | ((codepoint >> 6) & 0x3F)));
                    res.push_back(static_cast<char>(0x80 | (codepoint & 0x3F)));
                }
                k += 5;
            } else {
                switch (str[k + 1]) {
                case '"':
                case '\\':
                case '/':
                    res.push_back(str[k + 1]);
                    k++;
                    break;
                case 'b':
                    res.push_back('\b');
                    k++;
                    break;
                case 'f':
                    res.push_back('\f');
                    k++;
                    break;
                case 'n':
                    res.push_back('\n');
                    k++;
                    break;
                case 'r':
                    res.push_back('\r');
                    k++;
                    break;
                case 't':
                    res.push_back('\t');
                    k++;
                    break;
                default:
                    res.push_back(str[k]);
                    break;
                }
            }
        } else {
            res.push_back(str[k]);
        }
    }
    return res;
}
//=============================================================================
static bool
convertToJsonVariable(const std::string& jsonString, const jsmntok_t& token, JsonVariable& jsVar)
{
    bool res = false;
    std::string strValue(jsonString.substr(token.start, token.end - token.start));
    if (strValue == "null") {
        jsVar.jsonVariableType = JSON_TO_NELSON_EMPTY_MATRIX;
        jsVar.scalarDouble = std::nan("NaN");
        return true;
    }
    if (strValue == "NaN") {
        jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
        jsVar.scalarDouble = std::nan("NaN");
        return true;
    }
    if (strValue == "-Inf") {
        jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
        jsVar.scalarDouble = returnInfinity(false);
        return true;
    }
    if (strValue == "Inf") {
        jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
        jsVar.scalarDouble = returnInfinity(true);
        return true;
    }
    if (token.type == JSMN_STRING) {
        jsVar.jsonVariableType = JSON_TO_NELSON_STRING;
        jsVar.scalarString = decodeCharacters(strValue);
        return true;
    }
    if (token.type == JSMN_PRIMITIVE) {
        if (strValue == "false") {
            jsVar.jsonVariableType = JSON_TO_NELSON_LOGICAL;
            jsVar.scalarLogical = false;
            return true;
        }
        if (strValue == "true") {
            jsVar.jsonVariableType = JSON_TO_NELSON_LOGICAL;
            jsVar.scalarLogical = true;
            return true;
        }
        double val;
        fast_float::parse_options options { fast_float::chars_format::json_or_infnan };
        auto answer = fast_float::from_chars_advanced(
            strValue.data(), strValue.data() + strValue.size(), val, options);
        if (answer.ec == std::errc()) {
            jsVar.jsonVariableType = JSON_TO_NELSON_DOUBLE;
            jsVar.scalarDouble = val;
            return true;
        }
        // If parsing failed, treat as string
        jsVar.jsonVariableType = JSON_TO_NELSON_STRING;
        jsVar.scalarString = decodeCharacters(strValue);
        return true;
    }
    return res;
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
        return ArrayOf::characterArrayConstructor(jsVar.scalarString);
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
            ArrayOf rval = jsonVariableToNelson(jsVar.map.at(jsVar.fieldnames[i])[j]);
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
            JsonVariable jsv = jsVar.scalarMap[jsVar.fieldnames[k]];
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
                fieldvalues.push_back(jsonVariableToNelson(jsVar.map.at(jsVar.fieldnames[k])[0]));
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
        dptr[k] = jsonVariableToNelson(jsVar.vectorJsonVariable[k]);
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
static JSON_TO_NELSON_Type
findCommonJsonVariableType(JsonVariable& jsVar)
{
    if (jsVar.vectorJsonVariable.empty()) {
        return JSON_TO_NELSON_UNDEFINED;
    }

    int countDouble = 0, countLogical = 0, countEmpty = 0, countStruct = 0, countOther = 0;
    for (const auto& element : jsVar.vectorJsonVariable) {
        switch (element.jsonVariableType) {
        case JSON_TO_NELSON_DOUBLE:
            ++countDouble;
            break;
        case JSON_TO_NELSON_LOGICAL:
            ++countLogical;
            break;
        case JSON_TO_NELSON_EMPTY_MATRIX:
            ++countEmpty;
            break;
        case JSON_TO_NELSON_STRUCT:
            ++countStruct;
            break;
        default:
            ++countOther;
            break;
        }
        if (countOther > 0)
            break; // early exit
    }

    const size_t total = jsVar.vectorJsonVariable.size();
    if (countOther > 0) {
        return JSON_TO_NELSON_UNDEFINED;
    }
    if (countLogical == total) {
        return JSON_TO_NELSON_LOGICAL;
    }
    if (countStruct == total) {
        return JSON_TO_NELSON_STRUCT;
    }
    if (countDouble + countEmpty == total) {
        // Only DOUBLE and/or EMPTY_MATRIX
        return countDouble ? JSON_TO_NELSON_DOUBLE : JSON_TO_NELSON_EMPTY_MATRIX;
    }
    if (countEmpty == total) {
        return JSON_TO_NELSON_EMPTY_MATRIX;
    }
    if (countDouble == total) {
        return JSON_TO_NELSON_DOUBLE;
    }
    return JSON_TO_NELSON_UNDEFINED;
}
//=============================================================================
static bool
transformStringArray(JsonVariable& jsVar, size_t totaldims)
{
    switch (jsVar.dims.size()) {
    case 1: {
        jsVar.vectorString.resize(totaldims * jsVar.vectorJsonVariable.size());
        parallelTransform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
            jsVar.vectorString.begin(), [](JsonVariable& val) { return val.scalarString; });
    } break;
    case 2: {
        jsVar.vectorString.resize(totaldims * jsVar.vectorJsonVariable.size());
        size_t rows = jsVar.dims[0];
        size_t cols = jsVar.dims[1];
        for (size_t idx = 0; idx < rows * cols; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            std::string val = jsVar.vectorJsonVariable[i].vectorString[j];
            jsVar.vectorString[j * rows + i] = val;
        }
    } break;
    default: {
        std::vector<std::string> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element.vectorString.begin(), element.vectorString.end());
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
            jsVar.vectorLogical.begin(), [](JsonVariable& val) { return val.scalarLogical; });
    } break;
    case 2: {
        jsVar.vectorLogical.resize(totaldims * jsVar.vectorJsonVariable.size());
        size_t rows = jsVar.dims[0];
        size_t cols = jsVar.dims[1];
        size_t total = rows * cols;
        for (size_t idx = 0; idx < total; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            logical val = jsVar.vectorJsonVariable[i].vectorLogical[j];
            jsVar.vectorLogical[j * rows + i] = val;
        }
    } break;
    default: {
        std::vector<logical> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element.vectorLogical.begin(), element.vectorLogical.end());
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
    switch (jsVar.dims.size()) {
    case 1: {
        jsVar.vectorDouble.resize(totaldims * jsVar.vectorJsonVariable.size());
        parallelTransform(jsVar.vectorJsonVariable.begin(), jsVar.vectorJsonVariable.end(),
            jsVar.vectorDouble.begin(), [](JsonVariable& val) { return val.scalarDouble; });
    } break;
    case 2: {
        jsVar.vectorDouble.resize(totaldims * jsVar.vectorJsonVariable.size());
        size_t rows = jsVar.dims[0];
        size_t cols = jsVar.dims[1];
        size_t total = rows * cols;
        for (size_t idx = 0; idx < total; ++idx) {
            size_t i = idx % rows;
            size_t j = idx / rows;
            double val = jsVar.vectorJsonVariable[i].vectorDouble[j];
            jsVar.vectorDouble[j * rows + i] = val;
        }
    } break;
    default: {
        std::vector<double> vectTemp;
        vectTemp.reserve(totaldims * jsVar.vectorJsonVariable.size());
        for (auto element : jsVar.vectorJsonVariable) {
            vectTemp.insert(
                vectTemp.end(), element.vectorDouble.begin(), element.vectorDouble.end());
        }
        size_t lastdimlen = jsVar.dims[jsVar.dims.size() - 1];
        size_t elementCount = 1;
        for (size_t dim : jsVar.dims) {
            elementCount *= dim;
        }
        size_t ymax = elementCount / lastdimlen;
        size_t k = 0;
        jsVar.vectorDouble.reserve(elementCount);
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
    for (const auto& elements : jsVar.vectorJsonVariable[0].fieldnames) {
        fieldnamesRef.push_back(elements);
    }
    for (size_t i = 1; i < jsVar.vectorJsonVariable.size(); ++i) {
        if (jsVar.vectorJsonVariable[i].fieldnames.size() != fieldnamesRef.size()) {
            jsVar.reduced = true;
            jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
            return true;
        }
        for (const auto& element : jsVar.vectorJsonVariable[i].fieldnames) {
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
                jsVar.map[name].push_back(var.scalarMap[name]);
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
                if (var.dims.size() != 0) {
                    jsVar.map[name].push_back(var.map[name][j]);
                } else {
                    jsVar.map[name].push_back(var.scalarMap[name]);
                }
            }
        }
    } break;
    default: {
        for (const auto& name : fieldnamesRef) {
            for (auto var : jsVar.vectorJsonVariable) {
                if (var.dims.size() != 0) {
                    jsVar.map[name].insert(
                        jsVar.map[name].end(), var.map[name].begin(), var.map[name].end());
                } else {
                    jsVar.map[name].push_back(var.vectorJsonVariable[0].scalarMap[name]);
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
importTokens(
    const std::string& jsonString, int& tokens_offset, const jsmntok_t* tokens, JsonVariable& jsVar)
{
    bool res = false;
    switch (tokens[tokens_offset].type) {
    case JSMN_UNDEFINED: {
        return false;
    } break;
    case JSMN_ARRAY: {
        int size = tokens[tokens_offset++].size;
        if (size == 0) {
            jsVar.jsonVariableType = JSON_TO_NELSON_EMPTY_MATRIX;
            jsVar.reduced = true;
            return true;
        }
        jsVar.jsonVariableType = JSON_TO_NELSON_ARRAY;
        jsVar.vectorJsonVariable.reserve(size);
        for (int i = 0; i < size; ++i) {
            JsonVariable jsElement;
            if (!importTokens(jsonString, tokens_offset, tokens, jsElement)) {
                return false;
            }
            jsVar.vectorJsonVariable.push_back(jsElement);
        }
        JSON_TO_NELSON_Type commonType = findCommonJsonVariableType(jsVar);
        if (commonType != JSON_TO_NELSON_UNDEFINED) {
            std::vector<size_t> refVar = jsVar.vectorJsonVariable[0].dims;
            size_t totaldims = 1;
            for (auto i : refVar) {
                totaldims *= i;
            }
            for (const auto& element : jsVar.vectorJsonVariable) {
                const std::vector<size_t>& dims = element.dims;
                if (refVar.size() != dims.size()) {
                    jsVar.reduced = true;
                    jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
                    return true;
                }
                for (size_t j = 0; j < refVar.size(); ++j) {
                    if (refVar[j] != dims[j]) {
                        jsVar.reduced = true;
                        jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
                        return true;
                    }
                }
            }
            switch (refVar.size()) {
            case 0: {
                jsVar.dims.push_back(jsVar.vectorJsonVariable.size());
            } break;
            case 1: {
                jsVar.dims.resize(2);
                jsVar.dims[0] = jsVar.vectorJsonVariable.size();
                jsVar.dims[1] = refVar[0];
            } break;
            default: {
                jsVar.dims = refVar;
                jsVar.dims.push_back(jsVar.vectorJsonVariable.size());
            } break;
            }
            switch (commonType) {
            case JSON_TO_NELSON_EMPTY_MATRIX: {
                return true;
            } break;
            case JSON_TO_NELSON_DOUBLE: {
                return transformDoubleArray(jsVar, totaldims);
            } break;
            case JSON_TO_NELSON_LOGICAL: {
                return transformLogicalArray(jsVar, totaldims);
            } break;
            case JSON_TO_NELSON_STRING: {
                return transformStringArray(jsVar, totaldims);
            } break;
            case JSON_TO_NELSON_STRUCT: {
                return transformStructArray(jsVar, totaldims);
            } break;
            default: {
            } break;
            }
        } else {
            jsVar.jsonVariableType = JSON_TO_NELSON_CELL;
            return true;
        }
    } break;
    case JSMN_OBJECT: {
        int size = tokens[tokens_offset++].size;
        jsVar.jsonVariableType = JSON_TO_NELSON_STRUCT;
        jsVar.scalarMap.reserve(size);
        for (int i = 0; i < size; ++i) {
            JsonVariable jsKey;
            if (!importTokens(jsonString, tokens_offset, tokens, jsKey)) {
                return false;
            }
            std::string key = MakeValidFieldname(jsKey.scalarString);
            JsonVariable jsValue;
            if (!importTokens(jsonString, tokens_offset, tokens, jsValue)) {
                return false;
            }
            auto it = jsVar.scalarMap.find(key);
            if (it == jsVar.scalarMap.end()) {
                jsVar.scalarMap[key] = jsValue;
                jsVar.fieldnames.push_back(key);
            } else {
                size_t idx = 1;
                std::string modifiedKey;
                bool found = false;
                do {
                    modifiedKey = key + "_" + std::to_string(idx);
                    auto it
                        = std::find(jsVar.fieldnames.begin(), jsVar.fieldnames.end(), modifiedKey);
                    found = (it != jsVar.fieldnames.end());
                    idx++;
                } while (found);
                jsVar.scalarMap[modifiedKey] = jsValue;
                jsVar.fieldnames.push_back(modifiedKey);
            }
        }
        return true;
    } break;
    case JSMN_STRING:
    case JSMN_PRIMITIVE: {
        return convertToJsonVariable(jsonString, tokens[tokens_offset++], jsVar);
    } break;
    }
    return res;
}
//=============================================================================
static std::wstring
getErrorMessage(int errorCode)
{
    std::wstring errorMessage;
    switch (errorCode) {
    case JSMN_ERROR_INVAL: {
        errorMessage = _W("Invalid character inside JSON string.");
    } break;
    case JSMN_ERROR_NOMEM: {
        errorMessage = _W("Not enough tokens were provided.");
    } break;
    case JSMN_ERROR_PART: {
        errorMessage = _W("The string is not a full JSON packet, more bytes expected.");
    } break;
    default: {
        errorMessage = _W("Unknow error.");
    } break;
    }
    return errorMessage;
}
//=============================================================================
ArrayOf
jsonDecode(const std::wstring& stringToDecode, std::wstring& errorMessage)
{
    int tokens_offset = 0;
    std::wstring _stringToDecode(stringToDecode);
    StringHelpers::trim(_stringToDecode);
    std::string jsonString = wstring_to_utf8(_stringToDecode);
    if (jsonString.empty()) {
        errorMessage = _W("valid JSON Object expected.");
        return {};
    }
    jsmn_parser parserJson;
    jsmn_init(&parserJson);
    int nbTokensOrError
        = jsmn_parse(&parserJson, jsonString.c_str(), jsonString.size(), nullptr, 0);
    if (nbTokensOrError == 0) {
        if (jsonString.empty()) {
            return ArrayOf::emptyConstructor();
        }
        errorMessage = _W("valid JSON Object expected.");
        return {};
    }
    if (nbTokensOrError > 0) {
        // init again the parser required.
        jsmn_init(&parserJson);
        auto* tokens = new jsmntok_t[nbTokensOrError + 1];
        int nbTokensUsed = jsmn_parse(
            &parserJson, jsonString.c_str(), jsonString.size(), tokens, nbTokensOrError + 1);
        if (nbTokensUsed == 0) {
            delete[] tokens;
            errorMessage = _W("valid JSON Object expected.");
            return {};
        }
        tokens_offset = 0;
        JsonVariable jsVar;
        bool converted = importTokens(jsonString, tokens_offset, tokens, jsVar);
        delete[] tokens;
        if (!converted) {
            errorMessage = _W("valid JSON Object expected.");
            return {};
        }
        return jsonVariableToNelson(jsVar);
    }
    errorMessage = getErrorMessage(nbTokensOrError);

    return ArrayOf::emptyConstructor();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
