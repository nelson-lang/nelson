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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <cmath>
#include <cstdio>
#include "JsonTableWriter.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
using Tokens = std::vector<std::string>;
//=============================================================================
static bool
columnTokens(const ArrayOf& column, indexType rows, bool preserveInfAndNaN, Tokens& tokens);
static std::string
floatingToken(double value, bool isSingle, bool preserveInfAndNaN);
static std::string
quoted(const std::string& text);
static std::string
assemble(
    const stringVector& keys, const std::vector<Tokens>& columns, indexType rows, bool prettyPrint);
//=============================================================================
void
WriteJsonTable(const std::wstring& filename, const stringVector& keys, const ArrayOfVector& columns,
    bool prettyPrint, bool preserveInfAndNaN, std::wstring& errorMessage)
{
    indexType rows = columns.empty() ? 0 : columns[0].dimensions().rows();
    std::vector<Tokens> tokens(columns.size());
    for (size_t k = 0; k < columns.size(); ++k) {
        if (!columnTokens(columns[k], rows, preserveInfAndNaN, tokens[k])) {
            errorMessage = _W("Type not supported for JSON export: ") + utf8_to_wstring(keys[k]);
            return;
        }
    }
    std::string text = assemble(keys, tokens, rows, prettyPrint);
#ifdef _MSC_VER
    FILE* file = _wfopen(filename.c_str(), L"wb");
#else
    FILE* file = fopen(wstring_to_utf8(filename).c_str(), "wb");
#endif
    if (file == nullptr) {
        errorMessage = _W("Cannot open file: ") + filename;
        return;
    }
    size_t written = fwrite(text.data(), 1, text.size(), file);
    fclose(file);
    if (written != text.size()) {
        errorMessage = _W("Cannot write file: ") + filename;
    }
}
//=============================================================================
template <class T>
static void
integerTokens(const ArrayOf& column, Tokens& values)
{
    const T* data = (const T*)column.dataPointer();
    for (indexType k = 0; k < column.elementCount(); ++k) {
        values[k] = fmt::format("{}", data[k]);
    }
}
//=============================================================================
bool
columnTokens(const ArrayOf& column, indexType rows, bool preserveInfAndNaN, Tokens& tokens)
{
    indexType count = column.elementCount();
    Tokens values(count);
    switch (column.dataClass()) {
    case NLS_DOUBLE:
    case NLS_SINGLE: {
        bool isSingle = column.dataClass() == NLS_SINGLE;
        for (indexType k = 0; k < count; ++k) {
            double value = isSingle ? (double)((const single*)column.dataPointer())[k]
                                    : ((const double*)column.dataPointer())[k];
            values[k] = floatingToken(value, isSingle, preserveInfAndNaN);
        }
    } break;
    case NLS_LOGICAL: {
        const logical* data = (const logical*)column.dataPointer();
        for (indexType k = 0; k < count; ++k) {
            values[k] = data[k] ? "true" : "false";
        }
    } break;
    case NLS_INT8:
        integerTokens<int8>(column, values);
        break;
    case NLS_UINT8:
        integerTokens<uint8>(column, values);
        break;
    case NLS_INT16:
        integerTokens<int16>(column, values);
        break;
    case NLS_UINT16:
        integerTokens<uint16>(column, values);
        break;
    case NLS_INT32:
        integerTokens<int32>(column, values);
        break;
    case NLS_UINT32:
        integerTokens<uint32>(column, values);
        break;
    case NLS_INT64:
        integerTokens<int64>(column, values);
        break;
    case NLS_UINT64:
        integerTokens<uint64>(column, values);
        break;
    case NLS_STRING_ARRAY: {
        const ArrayOf* data = (const ArrayOf*)column.dataPointer();
        for (indexType k = 0; k < count; ++k) {
            values[k] = data[k].isCharacterArray() ? quoted(data[k].asUtf8String()) : "null";
        }
    } break;
    case NLS_CELL_ARRAY: {
        const ArrayOf* data = (const ArrayOf*)column.dataPointer();
        for (indexType k = 0; k < count; ++k) {
            if (!data[k].isCharacterArray()) {
                return false;
            }
            values[k] = data[k].asUtf8String();
        }
    } break;
    default:
        return false;
    }
    indexType width = rows == 0 ? 0 : count / rows;
    tokens.resize(rows);
    for (indexType r = 0; r < rows; ++r) {
        if (width == 1) {
            tokens[r] = std::move(values[r]);
            continue;
        }
        std::string token = "[";
        for (indexType c = 0; c < width; ++c) {
            token += (c > 0 ? "," : "") + values[r + c * rows];
        }
        tokens[r] = token + "]";
    }
    return true;
}
//=============================================================================
std::string
floatingToken(double value, bool isSingle, bool preserveInfAndNaN)
{
    if (std::isnan(value)) {
        return preserveInfAndNaN ? "NaN" : "null";
    }
    if (std::isinf(value)) {
        if (!preserveInfAndNaN) {
            return "null";
        }
        return value < 0 ? "-Inf" : "Inf";
    }
    if (std::trunc(value) == value && std::fabs(value) < 1e15) {
        return fmt::format("{}", (long long)value);
    }
    return isSingle ? fmt::format("{}", (float)value) : fmt::format("{}", value);
}
//=============================================================================
std::string
quoted(const std::string& text)
{
    std::string result;
    result.reserve(text.size() + 2);
    result.push_back('"');
    for (char c : text) {
        switch (c) {
        case '"':
            result += "\\\"";
            break;
        case '\\':
            result += "\\\\";
            break;
        case '\n':
            result += "\\n";
            break;
        case '\r':
            result += "\\r";
            break;
        case '\t':
            result += "\\t";
            break;
        case '\b':
            result += "\\b";
            break;
        case '\f':
            result += "\\f";
            break;
        default:
            if ((unsigned char)c < 0x20) {
                result += fmt::format("\\u{:04x}", (unsigned int)(unsigned char)c);
            } else {
                result.push_back(c);
            }
            break;
        }
    }
    result.push_back('"');
    return result;
}
//=============================================================================
std::string
assemble(
    const stringVector& keys, const std::vector<Tokens>& columns, indexType rows, bool prettyPrint)
{
    if (rows == 0) {
        return "[]";
    }
    std::vector<std::string> quotedKeys(keys.size());
    for (size_t k = 0; k < keys.size(); ++k) {
        quotedKeys[k] = quoted(keys[k]) + (prettyPrint ? ": " : ":");
    }
    const char* rowOpen = prettyPrint ? "    {\n" : "{";
    const char* rowClose = prettyPrint ? "\n    }" : "}";
    const char* memberIndent = prettyPrint ? "        " : "";
    const char* memberSeparator = prettyPrint ? ",\n" : ",";
    const char* rowSeparator = prettyPrint ? ",\n" : ",";
    std::string text = prettyPrint ? "[\n" : "[";
    for (indexType r = 0; r < rows; ++r) {
        if (r > 0) {
            text += rowSeparator;
        }
        if (columns.empty()) {
            text += prettyPrint ? "    {}" : "{}";
            continue;
        }
        text += rowOpen;
        for (size_t k = 0; k < columns.size(); ++k) {
            if (k > 0) {
                text += memberSeparator;
            }
            text += memberIndent;
            text += quotedKeys[k];
            text += columns[k][r];
        }
        text += rowClose;
    }
    text += prettyPrint ? "\n]" : "]";
    return text;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
