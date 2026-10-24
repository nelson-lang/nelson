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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include <cmath>
#include <complex>
#include <functional>
#include <unordered_map>
#include "WebParameters.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
protectCharacters(const std::string& str);
static std::string
scalarValueToString(ArrayOf& value);
static std::string
convertToOutputFormat(const std::string& name, indexType k, indexType nbElements,
    const std::wstring& arrayOutputFormat, const std::string& s);
static std::string
arrayValueToString(const std::string& name, ArrayOf& value, const std::wstring& arrayOutputFormat);
static std::string
arrayOfToString(const std::string& name, ArrayOf& value, WebOptions& options);
//=============================================================================
std::string
urlEscape(const std::string& text)
{
    static const char hex[] = "0123456789ABCDEF";
    std::string escaped;
    escaped.reserve(text.size() * 3);
    for (unsigned char c : text) {
        const bool unreserved = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
            || (c >= '0' && c <= '9') || c == '-' || c == '_' || c == '.' || c == '~';
        if (unreserved) {
            escaped.push_back(static_cast<char>(c));
        } else {
            escaped.push_back('%');
            escaped.push_back(hex[c >> 4]);
            escaped.push_back(hex[c & 0x0F]);
        }
    }
    return escaped;
}
//=============================================================================
static std::string
protectCharacters(const std::string& str)
{
    // http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
    static const std::unordered_map<char, std::string> unsafeCharacters = { { '<', "%3C" },
        { '>', "%3E" }, { '#', "%23" }, { '{', "%7B" }, { '}', "%7D" }, { '|', "%7C" },
        { '\\', "%5C" }, { '^', "%5E" }, { '~', "%7E" }, { '[', "%5B" }, { ']', "%5D" },
        { '`', "%60" }, { '+', "%20" }, { '/', "%2F" }, { '?', "%3F" }, { '&', "%26" } };
    std::string safeString;
    safeString.reserve(str.size());
    for (char c : str) {
        auto found = unsafeCharacters.find(c);
        if (found != unsafeCharacters.end()) {
            safeString.append(found->second);
        } else {
            safeString.push_back(c);
        }
    }
    return safeString;
}
//=============================================================================
template <typename T>
static std::string
realToString(T v)
{
    if (std::floor(v) == std::ceil(v)) {
        return fmt::to_string(int64(v));
    }
    return fmt::to_string(v);
}
//=============================================================================
template <typename T>
static std::string
complexToString(const std::complex<T>& v)
{
    std::string s = realToString(v.real());
    if (v.imag() > 0) {
        s += "+";
    }
    s += realToString(v.imag());
    s += "i";
    return s;
}
//=============================================================================
static std::string
scalarValueToString(ArrayOf& value)
{
    std::string s;
    switch (value.dataClass()) {
    case NLS_LOGICAL: {
        s = (value.asLogicalScalar() ? "1" : "0");
    } break;
    case NLS_CHAR:
    case NLS_STRING_ARRAY: {
        s = value.asUtf8String();
    } break;
    case NLS_DOUBLE: {
        s = realToString(value.asDoubleScalar());
    } break;
    case NLS_DCOMPLEX: {
        s = complexToString(value.asDoubleComplexScalar());
    } break;
    case NLS_SINGLE: {
        s = realToString(value.asSingleScalar());
    } break;
    case NLS_SCOMPLEX: {
        s = complexToString(value.asSingleComplexScalar());
    } break;
    case NLS_UINT8: {
        s = fmt::to_string(value.asUint8Scalar());
    } break;
    case NLS_INT8: {
        s = fmt::to_string(value.asInt8Scalar());
    } break;
    case NLS_UINT16: {
        s = fmt::to_string(value.asUint16Scalar());
    } break;
    case NLS_INT16: {
        s = fmt::to_string(value.asInt16Scalar());
    } break;
    case NLS_UINT32: {
        s = fmt::to_string(value.asUint32Scalar());
    } break;
    case NLS_INT32: {
        s = fmt::to_string(value.asInt32Scalar());
    } break;
    case NLS_UINT64: {
        s = fmt::to_string(value.asUint64Scalar());
    } break;
    case NLS_INT64: {
        s = fmt::to_string(value.asInt64Scalar());
    } break;
    default: {
        Error(L"Nelson:webREST:typeNotSupported", L"Type not supported.");
    } break;
    }
    return urlEscape(s);
}
//=============================================================================
static std::string
convertToOutputFormat(const std::string& name, indexType k, indexType nbElements,
    const std::wstring& arrayOutputFormat, const std::string& s)
{
    if (arrayOutputFormat == L"csv") {
        return (k == 0) ? s : "," + s;
    }
    if (arrayOutputFormat == L"json") {
        if (k == 0) {
            return protectCharacters("[" + s);
        }
        if (k == nbElements - 1) {
            return "," + protectCharacters(s + "]");
        }
        return "," + protectCharacters(s);
    }
    if (arrayOutputFormat == L"repeating") {
        std::string t = protectCharacters(name) + "=" + protectCharacters(s);
        return (k == 0) ? t : "&" + t;
    }
    if (arrayOutputFormat == L"php") {
        std::string t = protectCharacters(name + "[]") + "=" + protectCharacters(s);
        return (k == 0) ? t : "&" + t;
    }
    return {};
}
//=============================================================================
template <typename T>
static std::string
appendElements(const std::string& name, ArrayOf& value, const std::wstring& arrayOutputFormat,
    const std::function<std::string(const T*, indexType)>& elementToString)
{
    std::string output;
    const T* ptr = static_cast<const T*>(value.dataPointer());
    const indexType nbElements = value.dimensions().elementCount();
    for (indexType k = 0; k < nbElements; k++) {
        output += convertToOutputFormat(
            name, k, nbElements, arrayOutputFormat, elementToString(ptr, k));
    }
    return output;
}
//=============================================================================
static std::string
arrayValueToString(const std::string& name, ArrayOf& value, const std::wstring& arrayOutputFormat)
{
    std::string output;
    if (arrayOutputFormat == L"csv" || arrayOutputFormat == L"json") {
        output = urlEscape(name) + "=";
    }
    switch (value.dataClass()) {
    case NLS_LOGICAL:
        return output
            + appendElements<logical>(name, value, arrayOutputFormat,
                [](const logical* ptr, indexType k) { return std::string(ptr[k] ? "1" : "0"); });
    case NLS_CHAR:
        return urlEscape(name + "=" + value.asUtf8String());
    case NLS_STRING_ARRAY:
        return output
            + appendElements<ArrayOf>(
                name, value, arrayOutputFormat, [](const ArrayOf* ptr, indexType k) {
                    return ptr[k].isCharacterArray() ? ptr[k].asUtf8String() : std::string("NaN");
                });
    case NLS_CELL_ARRAY: {
        const auto* ptr = static_cast<const ArrayOf*>(value.dataPointer());
        const indexType nbElements = value.dimensions().elementCount();
        for (indexType k = 0; k < nbElements; k++) {
            ArrayOf v = ptr[k];
            output += arrayValueToString(name, v, arrayOutputFormat);
        }
        return output;
    }
    case NLS_DOUBLE:
        return output
            + appendElements<double>(name, value, arrayOutputFormat,
                [](const double* ptr, indexType k) { return realToString(ptr[k]); });
    case NLS_DCOMPLEX:
        return output
            + appendElements<double>(
                name, value, arrayOutputFormat, [](const double* ptr, indexType k) {
                    return complexToString(std::complex<double>(ptr[2 * k], ptr[2 * k + 1]));
                });
    case NLS_SINGLE:
        return output
            + appendElements<single>(name, value, arrayOutputFormat,
                [](const single* ptr, indexType k) { return realToString(ptr[k]); });
    case NLS_SCOMPLEX:
        return output
            + appendElements<single>(
                name, value, arrayOutputFormat, [](const single* ptr, indexType k) {
                    return complexToString(std::complex<single>(ptr[2 * k], ptr[2 * k + 1]));
                });
    case NLS_UINT8:
        return output
            + appendElements<uint8>(name, value, arrayOutputFormat,
                [](const uint8* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_INT8:
        return output
            + appendElements<int8>(name, value, arrayOutputFormat,
                [](const int8* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_UINT16:
        return output
            + appendElements<uint16>(name, value, arrayOutputFormat,
                [](const uint16* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_INT16:
        return output
            + appendElements<int16>(name, value, arrayOutputFormat,
                [](const int16* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_UINT32:
        return output
            + appendElements<uint32>(name, value, arrayOutputFormat,
                [](const uint32* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_INT32:
        return output
            + appendElements<int32>(name, value, arrayOutputFormat,
                [](const int32* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_UINT64:
        return output
            + appendElements<uint64>(name, value, arrayOutputFormat,
                [](const uint64* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    case NLS_INT64:
        return output
            + appendElements<int64>(name, value, arrayOutputFormat,
                [](const int64* ptr, indexType k) { return fmt::to_string(ptr[k]); });
    default:
        Error(L"Nelson:webREST:typeNotSupported", L"Type not supported.");
    }
    return output;
}
//=============================================================================
static std::string
arrayOfToString(const std::string& name, ArrayOf& value, WebOptions& options)
{
    if (value.isEmpty()) {
        return name;
    }
    if (value.isScalar() || value.isRowVectorCharacterArray() || value.isStringArray()) {
        return name + "=" + scalarValueToString(value);
    }
    return arrayValueToString(name, value, options.getArrayformat());
}
//=============================================================================
std::string
formEncode(const stringVector& names, const ArrayOfVector& values, WebOptions& options)
{
    std::string form;
    for (size_t k = 0; k < names.size(); ++k) {
        ArrayOf valueArrayOf = values[k];
        if (k != 0) {
            form.append("&");
        }
        form.append(arrayOfToString(urlEscape(names[k]), valueArrayOf, options));
    }
    return form;
}
//=============================================================================
std::string
encodeUrl(const std::wstring& url, const stringVector& names, const ArrayOfVector& values,
    WebOptions& options)
{
    std::string urlEncoded(wstring_to_utf8(url));
    std::string form = formEncode(names, values, options);
    if (!form.empty()) {
        if (!StringHelpers::ends_with(urlEncoded, "?")) {
            urlEncoded.append("?");
        }
        urlEncoded.append(form);
    }
    return urlEncoded;
}
//=============================================================================
stringVector
buildHeaderLines(const WebOptions& options)
{
    stringVector lines;
    const std::wstring encoding = options.getCharacterEncoding();
    if (encoding == L"UTF-8" || encoding == L"US-ASCII" || encoding == L"latin1"
        || encoding == L"Shift_JIS" || encoding == L"ISO-8859-1") {
        lines.push_back("charsets: " + wstring_to_utf8(encoding));
    }
    if (!options.getKeyName().empty() && options.getKeyValue().isEmpty()) {
        lines.push_back(wstring_to_utf8(options.getKeyName()) + ";");
    }
    if (!options.getMediaType().empty()) {
        lines.push_back("Content-Type: " + wstring_to_utf8(options.getMediaType()));
    }
    wstringVector headerFields = options.getHeaderFields();
    size_t nbFields = headerFields.size();
    if (nbFields > 0) {
        if (nbFields % 2) {
            Error(L"Nelson:weboptions:invalidHeaderFields:invalidHeaderFieldsSize",
                L"Invalid HeaderFields size.");
        }
        size_t nbFieldnames = nbFields / 2;
        for (size_t k = 0; k < nbFieldnames; k++) {
            lines.push_back(wstring_to_utf8(headerFields[k]) + ": "
                + wstring_to_utf8(headerFields[k + nbFieldnames]));
        }
    }
    return lines;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
