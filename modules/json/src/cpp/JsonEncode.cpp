//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "JsonEncode.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <iomanip>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string jsonString;
//=============================================================================
static void
json_append_char(char c)
{
    jsonString.push_back(c);
}
//=============================================================================
static void
json_append_string(const std::string& str)
{
    jsonString.append(str);
}
//=============================================================================
static bool
isSupportedType(ArrayOf ValueToEncode)
{
    if (ValueToEncode.isClassStruct()) {
        return false;
    }
    if (ValueToEncode.isSparse()) {
        return false;
    }
    switch (ValueToEncode.getDataClass()) {
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
encode_character(wchar_t ch)
{
    switch (ch) {
    case L'"':
    case L'\\': {
        json_append_char('\\');
        std::wstring wstr;
        wstr.push_back(ch);
        std::string str = wstring_to_utf8(wstr);
        json_append_string(str);
    } break;
    case L'\b':
        json_append_string("\\b");
        break;
    case L'\f':
        json_append_string("\\f");
        break;
    case L'\n':
        json_append_string("\\n");
        break;
    case L'\r':
        json_append_string("\\r");
        break;
    case L'\t':
        json_append_string("\\t");
        break;
    default: {
        if ((ch > 13) && (ch < 32)) {
            char buff[1024];
            snprintf(buff, sizeof(buff), "\\u%04hx", ch);
            std::string buffAsStdStr = buff;
            json_append_string(buffAsStdStr);
        } else {
            std::wstring wstr;
            wstr.push_back(ch);
            std::string str = wstring_to_utf8(wstr);
            json_append_string(str);
        }
    } break;
    }
}
//=============================================================================
static void
encode_double(double val, bool convertNanInf)
{
    if (std::isnan(val)) {
        if (convertNanInf) {
            json_append_string("NaN,");
        } else {
            json_append_string("null,");
        }
    } else if (std::isinf(val)) {
        if (convertNanInf) {
            if (val > 0) {
                json_append_string("Inf,");
            } else {
                json_append_string("-Inf,");
            }
        } else {
            json_append_string("null,");
        }
    } else {
        std::stringstream stream;
        stream << std::setprecision(std::numeric_limits<double>::digits10 + 1) << val;
        std::string s = stream.str();
        json_append_string(s + ",");
    }
}
//=============================================================================
static void
encode_single(single val, bool convertNanInf)
{
    if (std::isnan(val)) {
        if (convertNanInf) {
            json_append_string("NaN,");
        } else {
            json_append_string("null,");
        }
    } else if (std::isinf(val)) {
        if (convertNanInf) {
            if (val > 0) {
                json_append_string("Inf,");
            } else {
                json_append_string("-Inf,");
            }
        } else {
            json_append_string("null,");
        }
    } else {
        encode_double(static_cast<double>(val), convertNanInf);
    }
}
//=============================================================================
static void
encode_array(ArrayOf ValueToEncode, bool close)
{
    if (ValueToEncode.isCell()) {
        if (close) {
            json_append_char(']');
        } else {
            json_append_char('[');
        }
    } else {
        indexType nbElements = ValueToEncode.getDimensions().getElementCount();
        if (nbElements > 1) {
            if (ValueToEncode.getDataClass() != NLS_CHAR) {
                if (close) {
                    json_append_char(']');
                } else {
                    json_append_char('[');
                }
            } else {
                if (!ValueToEncode.isRowVector() && !ValueToEncode.isColumnVector()) {
                    if (close) {
                        json_append_char(']');
                    } else {
                        json_append_char('[');
                    }
                }
            }
        }
    }
}
//=============================================================================
template <class T>
static void
jsonEncodeInteger(const ArrayOf& ValueToEncode, std::string format)
{
    auto* ptr = (T*)ValueToEncode.getDataPointer();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
            char buff[1024];
            snprintf(buff, sizeof(buff), format.c_str(), ptr[i]);
            std::string buffAsStdStr = buff;
            json_append_string(buffAsStdStr);
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getDimensions().getRows();
        indexType cols = ValueToEncode.getDimensions().getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char('[');
            for (int j = 0; j < cols; ++j) {
                char buff[1024];
                snprintf(buff, sizeof(buff), format.c_str(), ptr[j * rows + i]);
                std::string buffAsStdStr = buff;
                json_append_string(buffAsStdStr);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char('[');
            for (int j = 0; j < lastdimlen; ++j) {
                char buff[1024];
                snprintf(buff, sizeof(buff), format.c_str(), ptr[j * ymax + i]);
                std::string buffAsStdStr = buff;
                json_append_string(buffAsStdStr);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    }
}
//=============================================================================
static void
jsonEncodeDouble(const ArrayOf& ValueToEncode, bool convertNanInf)
{
    auto* ptr = (double*)ValueToEncode.getDataPointer();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
            encode_double(ptr[i], convertNanInf);
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getDimensions().getRows();
        indexType cols = ValueToEncode.getDimensions().getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char('[');
            for (int j = 0; j < cols; ++j) {
                encode_double(ptr[j * rows + i], convertNanInf);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char('[');
            for (int j = 0; j < lastdimlen; ++j) {
                encode_double(ptr[j * ymax + i], convertNanInf);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    }
}
//=============================================================================
static void
jsonEncodeSingle(const ArrayOf& ValueToEncode, bool convertNanInf)
{
    auto* ptr = (single*)ValueToEncode.getDataPointer();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
            encode_single(ptr[i], convertNanInf);
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getDimensions().getRows();
        indexType cols = ValueToEncode.getDimensions().getColumns();
        for (int i = 0; i < rows; ++i) {
            json_append_char('[');
            for (int j = 0; j < cols; ++j) {
                encode_single(ptr[j * rows + i], convertNanInf);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char('[');
            for (int j = 0; j < lastdimlen; ++j) {
                encode_single(ptr[j * ymax + i], convertNanInf);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    }
}
//=============================================================================
static void
jsonEncodeCharacters(const ArrayOf& ValueToEncode)
{
    std::wstring strw = ValueToEncode.getContentAsArrayOfCharacters();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        jsonString.reserve(jsonString.size() + strw.size() + 2);
        json_append_char('"');
        for (wchar_t i : strw) {
            encode_character(i);
        }
        json_append_char('"');
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getDimensions().getRows();
        indexType cols = ValueToEncode.getDimensions().getColumns();
        jsonString.reserve(jsonString.size() + strw.size() + 2);
        for (int i = 0; i < rows; ++i) {
            json_append_char('"');
            for (int j = 0; j < cols; ++j) {
                encode_character(strw[j * rows + i]);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("\",");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (int i = 0; i < ymax; ++i) {
            json_append_char('\"');
            for (int j = 0; j < lastdimlen; ++j) {
                wchar_t ch = strw[i * ymax + j];
                encode_character(ch);
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("\",");
        }
    }
}
//=============================================================================
static void
jsonEncodeLogical(const ArrayOf& ValueToEncode)
{
    auto* ptr = (logical*)ValueToEncode.getDataPointer();
    if (ValueToEncode.isRowVector() || ValueToEncode.isColumnVector()) {
        for (indexType i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
            if (ptr[i] == 0) {
                json_append_string("false,");
            } else {
                json_append_string("true,");
            }
        }
    } else if (ValueToEncode.is2D()) {
        indexType rows = ValueToEncode.getDimensions().getRows();
        indexType cols = ValueToEncode.getDimensions().getColumns();
        for (indexType i = 0; i < rows; ++i) {
            json_append_char('[');
            for (indexType j = 0; j < cols; ++j) {
                if (ptr[i] == 0) {
                    json_append_string("false,");
                } else {
                    json_append_string("true,");
                }
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    } else {
        Dimensions dims = ValueToEncode.getDimensions();
        indexType lastdimlen = dims.getDimensionLength(dims.getLength() - 1);
        indexType ymax = dims.getElementCount() / lastdimlen;
        for (indexType i = 0; i < ymax; ++i) {
            json_append_char('[');
            for (indexType j = 0; j < lastdimlen; ++j) {
                if (ptr[i] == 0) {
                    json_append_string("false,");
                } else {
                    json_append_string("true,");
                }
            }
            if (boost::algorithm::ends_with(jsonString, L",")) {
                jsonString.pop_back();
            }
            json_append_string("],");
        }
    }
}
//=============================================================================
static ArrayOf
jsonEncodeInternal(ArrayOf ValueToEncode, bool convertNanInf, std::wstring& errorMessage)
{
    if (!isSupportedType(ValueToEncode)) {
        errorMessage = _W("Unsupported type to convert as JSON.");
        return ArrayOf();
    }
    if (ValueToEncode.isEmpty()) {
        if (ValueToEncode.getDataClass() == NLS_CHAR) {
            json_append_string("\"\"");
        } else {
            json_append_string("[]");
        }
    } else {
        encode_array(ValueToEncode, false);
        switch (ValueToEncode.getDataClass()) {
        case NLS_DCOMPLEX:
        case NLS_SCOMPLEX:
        case NLS_HANDLE: {
            errorMessage = _W("Unsupported type to convert as JSON.");
            return ArrayOf();
        } break;
        case NLS_STRING_ARRAY:
        case NLS_CELL_ARRAY: {
            auto* elements = (ArrayOf*)ValueToEncode.getDataPointer();
            for (int i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
                jsonEncodeInternal(elements[i], convertNanInf, errorMessage);
                json_append_char(',');
            }
        } break;
        case NLS_STRUCT_ARRAY: {
            stringVector fieldnames = ValueToEncode.getFieldNames();
            for (int i = 0; i < ValueToEncode.getDimensions().getElementCount(); i++) {
                json_append_char('{');
                for (auto fieldname : fieldnames) {
                    ArrayOfVector values = ValueToEncode.getFieldAsList(fieldname);
                    if (!values.empty()) {
                        json_append_string("\"" + fieldname + "\":");
                        jsonEncodeInternal(values[i], convertNanInf, errorMessage);
                        json_append_char(',');
                    }
                }
                if (boost::algorithm::ends_with(jsonString, L",")) {
                    jsonString.pop_back();
                }
                json_append_char('}');
                json_append_char(',');
            }
        } break;
        case NLS_LOGICAL: {
            jsonEncodeLogical(ValueToEncode);
        } break;
        case NLS_UINT8: {
            jsonEncodeInteger<uint8>(ValueToEncode, "%u,");
        } break;
        case NLS_INT8: {
            jsonEncodeInteger<int8>(ValueToEncode, "%i,");
        } break;
        case NLS_UINT16: {
            jsonEncodeInteger<uint16>(ValueToEncode, "%u,");
        } break;
        case NLS_INT16: {
            jsonEncodeInteger<int16>(ValueToEncode, "%i,");
        } break;
        case NLS_UINT32: {
            jsonEncodeInteger<uint32>(ValueToEncode, "%u,");
        } break;
        case NLS_INT32: {
            jsonEncodeInteger<int32>(ValueToEncode, "%i,");
        } break;
        case NLS_UINT64: {
            jsonEncodeInteger<uint64>(ValueToEncode, "%llu,");
        } break;
        case NLS_INT64: {
            jsonEncodeInteger<uint64>(ValueToEncode, "%lli,");
        } break;
        case NLS_SINGLE: {
            jsonEncodeSingle(ValueToEncode, convertNanInf);
        } break;
        case NLS_DOUBLE: {
            jsonEncodeDouble(ValueToEncode, convertNanInf);
        } break;
        case NLS_CHAR: {
            jsonEncodeCharacters(ValueToEncode);
        } break;
        default: {
            errorMessage = _W("Unsupported type to convert as JSON.");
            return ArrayOf();
        } break;
        }
        if (boost::algorithm::ends_with(jsonString, L",")) {
            jsonString.pop_back();
        }
        encode_array(ValueToEncode, true);
    }
    return ArrayOf::characterArrayConstructor(jsonString);
}
//=============================================================================
ArrayOf
jsonEncode(ArrayOf ValueToEncode, bool convertNanInf, std::wstring& errorMessage)
{
    jsonString.clear();
    ArrayOf res = jsonEncodeInternal(ValueToEncode, convertNanInf, errorMessage);
    jsonString.clear();
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
