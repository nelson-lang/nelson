//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <iomanip>
#include <sstream>
#include <cstring>
#include "StringHelpers.hpp"
#include "nlsBuildConfig.h"
#include "JsonEncode.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
json_append_char(std::wstring& jsonString, wchar_t c)
{
    jsonString.push_back(c);
}
//=============================================================================
static void
json_append_string(std::wstring& jsonString, const std::wstring& str)
{
    jsonString.append(str);
}
//=============================================================================
static bool
isSupportedType(const ArrayOf& ValueToEncode)
{
    if (ValueToEncode.isClassType()) {
        return false;
    }
    if (ValueToEncode.isSparse()) {
        return false;
    }
    switch (ValueToEncode.getDataClass()) {
    case NLS_FUNCTION_HANDLE:
    case NLS_CLASS_ARRAY:
    case NLS_GO_HANDLE:
    case NLS_HANDLE:
        return false;
    case NLS_STRING_ARRAY:
    case NLS_CELL_ARRAY:
    case NLS_STRUCT_ARRAY:
    case NLS_LOGICAL:
    case NLS_UINT8:
    case NLS_INT8:
    case NLS_UINT16:
    case NLS_INT16:
    case NLS_UINT32:
    case NLS_INT32:
    case NLS_UINT64:
    case NLS_INT64:
    case NLS_SINGLE:
    case NLS_DOUBLE:
    case NLS_CHAR:
    case NLS_MISSING_ARRAY:
        return true;
    case NLS_SCOMPLEX:
        return false;
    case NLS_DCOMPLEX:
        return false;
    default:
        return false;
    }
    return false;
}
//=============================================================================
static void
encode_character(std::wstring& jsonString, wchar_t ch)
{
    switch (ch) {
    case L'"': {
        json_append_string(jsonString, L"\\\"");
    } break;
    case L'\\': {
        json_append_string(jsonString, L"\\\\");
    } break;
    case L'\b':
        json_append_string(jsonString, L"\\b");
        break;
    case L'\f':
        json_append_string(jsonString, L"\\f");
        break;
    case L'\n':
        json_append_string(jsonString, L"\\n");
        break;
    case L'\r':
        json_append_string(jsonString, L"\\r");
        break;
    case L'\t':
        json_append_string(jsonString, L"\\t");
        break;
    default: {
        if ((ch > 13) && (ch < 32)) {
            json_append_string(jsonString, fmt::sprintf(std::wstring(L"\\u%04hx"), ch));
        } else {
            std::wstring wstr;
            wstr.push_back(ch);
            json_append_string(jsonString, wstr);
        }
    } break;
    }
}
//=============================================================================
static void
encode_double(std::wstring& jsonString, double val, bool convertNanInf)
{
    if (std::isnan(val)) {
        if (convertNanInf) {
            json_append_string(jsonString, L"NaN,");
        } else {
            json_append_string(jsonString, L"null,");
        }
    } else if (std::isinf(val)) {
        if (convertNanInf) {
            if (val > 0) {
                json_append_string(jsonString, L"Inf,");
            } else {
                json_append_string(jsonString, L"-Inf,");
            }
        } else {
            json_append_string(jsonString, L"null,");
        }
    } else {
        std::wstringstream stream;
        stream << std::setprecision(std::numeric_limits<double>::digits10 + 1) << val;
        std::wstring s = stream.str();
        json_append_string(jsonString, s + L",");
    }
}
//=============================================================================
static void
encode_single(std::wstring& jsonString, single val, bool convertNanInf)
{
    if (std::isnan(val)) {
        if (convertNanInf) {
            json_append_string(jsonString, L"NaN,");
        } else {
            json_append_string(jsonString, L"null,");
        }
    } else if (std::isinf(val)) {
        if (convertNanInf) {
            if (val > 0) {
                json_append_string(jsonString, L"Inf,");
            } else {
                json_append_string(jsonString, L"-Inf,");
            }
        } else {
            json_append_string(jsonString, L"null,");
        }
    } else {
        encode_double(jsonString, static_cast<double>(val), convertNanInf);
    }
}
//=============================================================================
static void
encode_array(std::wstring& jsonString, const ArrayOf& ValueToEncode, bool close)
{
    if (ValueToEncode.isCell()) {
        if (close) {
            json_append_char(jsonString, L']');
        } else {
            json_append_char(jsonString, L'[');
        }
    } else {
        indexType nbElements = ValueToEncode.getElementCount();
        if (nbElements > 1) {
            if (ValueToEncode.getDataClass() != NLS_CHAR) {
                if (close) {
                    json_append_char(jsonString, L']');
                } else {
                    json_append_char(jsonString, L'[');
                }
            } else {
                if (!ValueToEncode.isRowVector() && !ValueToEncode.isColumnVector()) {
                    if (close) {
                        json_append_char(jsonString, L']');
                    } else {
                        json_append_char(jsonString, L'[');
                    }
                }
            }
        }
    }
}
//=============================================================================
template <class T>
static void
jsonEncodeInteger(
    std::wstring& jsonString, const ArrayOf& ValueToEncode, const std::wstring& format)
{
    auto* ptr = (T*)ValueToEncode.getDataPointer();
    // Pre-allocate string capacity to reduce reallocations
    // Estimate ~20 chars per integer
    ompIndexType elementCount = ValueToEncode.getElementCount();
    jsonString.reserve(jsonString.size() + elementCount * 20);
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (ompIndexType i = 0; i < elementCount; i++) {
            json_append_string(jsonString, fmt::sprintf(format, ptr[i]));
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char(jsonString, '[');
            for (int j = 0; j < cols; ++j) {
                json_append_string(jsonString, fmt::sprintf(format, ptr[j * rows + i]));
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char(jsonString, '[');
            for (int j = 0; j < lastdimlen; ++j) {
                json_append_string(jsonString, fmt::sprintf(format, ptr[j * ymax + i]));
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    }
}
//=============================================================================
static void
jsonEncodeDouble(std::wstring& jsonString, const ArrayOf& ValueToEncode, bool convertNanInf)
{
    auto* ptr = (double*)ValueToEncode.getDataPointer();
    // Pre-allocate string capacity to reduce reallocations
    // Estimate ~20 chars per double (conservative for scientific notation)
    ompIndexType elementCount = ValueToEncode.getElementCount();
    jsonString.reserve(jsonString.size() + elementCount * 20);

    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (ompIndexType i = 0; i < elementCount; i++) {
            encode_double(jsonString, ptr[i], convertNanInf);
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char(jsonString, L'[');
            for (int j = 0; j < cols; ++j) {
                encode_double(jsonString, ptr[j * rows + i], convertNanInf);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char(jsonString, L'[');
            for (int j = 0; j < lastdimlen; ++j) {
                encode_double(jsonString, ptr[j * ymax + i], convertNanInf);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    }
}
//=============================================================================
static void
jsonEncodeSingle(std::wstring& jsonString, const ArrayOf& ValueToEncode, bool convertNanInf)
{
    auto* ptr = (single*)ValueToEncode.getDataPointer();
    // Pre-allocate string capacity to reduce reallocations
    // Estimate ~20 chars per double (conservative for scientific notation)
    ompIndexType elementCount = ValueToEncode.getElementCount();
    jsonString.reserve(jsonString.size() + elementCount * 20);

    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (ompIndexType i = 0; i < elementCount; i++) {
            encode_single(jsonString, ptr[i], convertNanInf);
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char(jsonString, '[');
            for (int j = 0; j < cols; ++j) {
                encode_single(jsonString, ptr[j * rows + i], convertNanInf);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char(jsonString, L'[');
            for (int j = 0; j < lastdimlen; ++j) {
                encode_single(jsonString, ptr[j * ymax + i], convertNanInf);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    }
}
//=============================================================================
static void
jsonEncodeCharacters(std::wstring& jsonString, const ArrayOf& ValueToEncode)
{
    std::wstring strw = ValueToEncode.getContentAsArrayOfCharacters();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        jsonString.reserve(jsonString.size() + strw.size() + 2);
        json_append_char(jsonString, L'"');
        for (wchar_t i : strw) {
            encode_character(jsonString, i);
        }
        json_append_char(jsonString, L'"');
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        jsonString.reserve(jsonString.size() + strw.size() + 2);
        for (int i = 0; i < rows; ++i) {
            json_append_char(jsonString, L'"');
            for (int j = 0; j < cols; ++j) {
                encode_character(jsonString, strw[j * rows + i]);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"\",");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char(jsonString, L'\"');
            for (int j = 0; j < lastdimlen; ++j) {
                wchar_t ch = strw[i * ymax + j];
                encode_character(jsonString, ch);
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"\",");
        }
    }
}
//=============================================================================
static void
jsonEncodeMissing(std::wstring& jsonString, const ArrayOf& ValueToEncode)
{
    // Pre-allocate string capacity to reduce reallocations
    // Estimate ~20 chars per missing
    ompIndexType elementCount = ValueToEncode.getElementCount();
    jsonString.reserve(jsonString.size() + elementCount * 7);

    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (ompIndexType i = 0; i < elementCount; i++) {
            json_append_string(jsonString, L"null,");
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        for (indexType i = 0; i < rows; ++i) {
            json_append_char(jsonString, '[');
            for (indexType j = 0; j < cols; ++j) {
                json_append_string(jsonString, L"null,");
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (indexType i = 0; i < ymax; ++i) {
            json_append_char(jsonString, '[');
            for (indexType j = 0; j < lastdimlen; ++j) {
                json_append_string(jsonString, L"null,");
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    }
}
//=============================================================================
static void
jsonEncodeLogical(std::wstring& jsonString, const ArrayOf& ValueToEncode)
{
    auto* ptr = (logical*)ValueToEncode.getDataPointer();
    // Pre-allocate string capacity to reduce reallocations
    // Estimate ~20 chars per logical
    ompIndexType elementCount = ValueToEncode.getElementCount();
    jsonString.reserve(jsonString.size() + elementCount * 8);
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (ompIndexType i = 0; i < elementCount; i++) {
            if (ptr[i] == 0) {
                json_append_string(jsonString, L"false,");
            } else {
                json_append_string(jsonString, L"true,");
            }
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getRows();
        indexType cols = ValueToEncode.getColumns();
        for (indexType i = 0; i < rows; ++i) {
            json_append_char(jsonString, '[');
            for (indexType j = 0; j < cols; ++j) {
                if (ptr[i] == 0) {
                    json_append_string(jsonString, L"false,");
                } else {
                    json_append_string(jsonString, L"true,");
                }
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (indexType i = 0; i < ymax; ++i) {
            json_append_char(jsonString, '[');
            for (indexType j = 0; j < lastdimlen; ++j) {
                if (ptr[i] == 0) {
                    json_append_string(jsonString, L"false,");
                } else {
                    json_append_string(jsonString, L"true,");
                }
            }
            if (StringHelpers::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string(jsonString, L"],");
        }
    }
}
//=============================================================================
static ArrayOf
jsonEncodeInternal(
    std::wstring& jsonString, ArrayOf ValueToEncode, bool convertNanInf, std::wstring& errorMessage)
{
    if (!isSupportedType(ValueToEncode)) {
        errorMessage = _W("Unsupported type to convert as JSON.");
        return {};
    }
    if (ValueToEncode.isEmpty()) {
        if (ValueToEncode.getDataClass() == NLS_CHAR) {
            json_append_string(jsonString, L"\"\"");
        } else {
            json_append_string(jsonString, L"[]");
        }
    } else {
        encode_array(jsonString, ValueToEncode, false);
        switch (ValueToEncode.getDataClass()) {
        case NLS_DCOMPLEX:
        case NLS_SCOMPLEX:
        case NLS_GO_HANDLE:
        case NLS_HANDLE: {
            errorMessage = _W("Unsupported type to convert as JSON.");
            return {};
        } break;
        case NLS_MISSING_ARRAY: {
            jsonEncodeMissing(jsonString, ValueToEncode);
        } break;
        case NLS_STRING_ARRAY:
        case NLS_CELL_ARRAY: {
            auto* elements = (ArrayOf*)ValueToEncode.getDataPointer();
            indexType elementCount = ValueToEncode.getElementCount();
            for (indexType i = 0; i < elementCount; i++) {
                jsonEncodeInternal(jsonString, elements[i], convertNanInf, errorMessage);
                json_append_char(jsonString, L',');
            }
        } break;
        case NLS_STRUCT_ARRAY: {
            stringVector fieldnames = ValueToEncode.getFieldNames();
            indexType elementCount = ValueToEncode.getElementCount();
            for (int i = 0; i < elementCount; i++) {
                json_append_char(jsonString, L'{');
                for (const auto& fieldname : fieldnames) {
                    ArrayOfVector values = ValueToEncode.getFieldAsList(fieldname);
                    if (!values.empty()) {
                        json_append_string(jsonString, L"\"" + utf8_to_wstring(fieldname) + L"\":");
                        jsonEncodeInternal(jsonString, values[i], convertNanInf, errorMessage);
                        json_append_char(jsonString, L',');
                    }
                }
                if (StringHelpers::ends_with(jsonString, L",")) {
                    jsonString.pop_back();
                }
                json_append_char(jsonString, L'}');
                json_append_char(jsonString, L',');
            }
        } break;
        case NLS_LOGICAL: {
            jsonEncodeLogical(jsonString, ValueToEncode);
        } break;
        case NLS_UINT8: {
            jsonEncodeInteger<uint8>(jsonString, ValueToEncode, L"%u,");
        } break;
        case NLS_INT8: {
            jsonEncodeInteger<int8>(jsonString, ValueToEncode, L"%i,");
        } break;
        case NLS_UINT16: {
            jsonEncodeInteger<uint16>(jsonString, ValueToEncode, L"%u,");
        } break;
        case NLS_INT16: {
            jsonEncodeInteger<int16>(jsonString, ValueToEncode, L"%i,");
        } break;
        case NLS_UINT32: {
            jsonEncodeInteger<uint32>(jsonString, ValueToEncode, L"%u,");
        } break;
        case NLS_INT32: {
            jsonEncodeInteger<int32>(jsonString, ValueToEncode, L"%i,");
        } break;
        case NLS_UINT64: {
            jsonEncodeInteger<uint64>(jsonString, ValueToEncode, L"%llu,");
        } break;
        case NLS_INT64: {
            jsonEncodeInteger<uint64>(jsonString, ValueToEncode, L"%lli,");
        } break;
        case NLS_SINGLE: {
            jsonEncodeSingle(jsonString, ValueToEncode, convertNanInf);
        } break;
        case NLS_DOUBLE: {
            jsonEncodeDouble(jsonString, ValueToEncode, convertNanInf);
        } break;
        case NLS_CHAR: {
            jsonEncodeCharacters(jsonString, ValueToEncode);
        } break;
        default: {
            errorMessage = _W("Unsupported type to convert as JSON.");
            return {};
        } break;
        }
        if (StringHelpers::ends_with(jsonString, L",")) {
            jsonString.pop_back();
        }
        encode_array(jsonString, ValueToEncode, true);
    }
    return ArrayOf::characterArrayConstructor(jsonString);
}
//=============================================================================
ArrayOf
jsonEncode(const ArrayOf& ValueToEncode, bool convertNanInf, std::wstring& errorMessage)
{
    std::wstring jsonString;
    jsonString.reserve(4096);
    return jsonEncodeInternal(jsonString, ValueToEncode, convertNanInf, errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
