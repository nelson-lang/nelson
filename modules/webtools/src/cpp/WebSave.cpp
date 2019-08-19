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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#include <curl/curl.h>
#include <boost/filesystem/path.hpp>
#include <boost/filesystem.hpp>
#include <boost/unordered_map.hpp>
#include "WebSave.hpp"
#include "WebOptions.hpp"
#include "characters_encoding.hpp"
#include "dynamic_library.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static library_handle nlsGuiHandleDynamicLibrary = nullptr;
static bool bFirstDynamicLibraryCall = true;
static bool bHaveEventsLoop = false;
static boost::unordered_map<std::string, std::string> unsafeCharacters;
//=============================================================================
static std::string
protectCharacters(std::string str)
{
    // http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
    std::string safeString;
    safeString.reserve(str.size());
    if (unsafeCharacters.empty()) {
        unsafeCharacters["<"] = "%3C";
        unsafeCharacters[">"] = "%3E";
        unsafeCharacters["#"] = "%23";
        unsafeCharacters["{"] = "%7B";
        unsafeCharacters["}"] = "%7D";
        unsafeCharacters["|"] = "%7C";
        unsafeCharacters["\\"] = "%5C";
        unsafeCharacters["^"] = "%5E";
        unsafeCharacters["~"] = "%7E";
        unsafeCharacters["["] = "%5B";
        unsafeCharacters["]"] = "%5D";
        unsafeCharacters["`"] = "%60";
        unsafeCharacters["+"] = "%20";
        unsafeCharacters["/"] = "%2F";
        unsafeCharacters["?"] = "%3F";
        unsafeCharacters["&"] = "%26";
    }
    for (char c : str) {
        std::string s(1, c);
        boost::unordered_map<std::string, std::string>::const_iterator found
            = unsafeCharacters.find(s);
        if (found != unsafeCharacters.end()) {
            safeString.append(found->second);
        } else {
            safeString.append(s);
        }
    }
    return safeString;
}
//=============================================================================
static void
initGuiDynamicLibrary();
static void
ProcessEventsDynamicFunction(bool bWait);
//=============================================================================
static CURLcode
buildHeader(CURL* curlObject, WebOptions& options)
{
    stringVector lines;
    if (options.getCharacterEncoding() == L"UTF-8") {
        lines.push_back("charsets: UTF-8");
    }
    if (options.getCharacterEncoding() == L"US-ASCII") {
        lines.push_back("charsets: US-ASCII");
    }
    if (options.getCharacterEncoding() == L"latin1") {
        lines.push_back("charsets: latin1");
    }
    if (options.getCharacterEncoding() == L"Shift_JIS") {
        lines.push_back("charsets: Shift_JIS");
    }
    if (options.getCharacterEncoding() == L"ISO-8859-1") {
        lines.push_back("charsets: ISO-8859-1");
    }

    if (!options.getKeyName().empty()) {
        std::string name = wstring_to_utf8(options.getKeyName());
        ArrayOf valueArrayOf = options.getKeyValue();
        if (valueArrayOf.isEmpty()) {
            lines.push_back(name + ";");
        } else {
        }
    }

    if (options.getMediaType().empty()) {
        lines.push_back("Content-Type: " + wstring_to_utf8(options.getMediaType()));
    }

    struct curl_slist* chunk = nullptr;
    for (std::string l : lines) {
        chunk = curl_slist_append(chunk, l.c_str());
    }
    return curl_easy_setopt(curlObject, CURLOPT_HTTPHEADER, chunk);
}
//=============================================================================
static CURLcode
setCurlWebOptions(CURL* curlObject, WebOptions& options)
{
    CURLcode res = CURLE_FAILED_INIT;
    if (curlObject != nullptr) {
        std::string userAgent = wstring_to_utf8(options.getUserAgent());
        res = curl_easy_setopt(curlObject, CURLOPT_USERAGENT, userAgent.c_str());
        if (res != CURLE_OK) {
            return res;
        }

        if (!options.getUsername().empty()) {
            std::string userpwd = wstring_to_utf8(options.getUsername()) + ":"
                + wstring_to_utf8(options.getPassword());
            res = curl_easy_setopt(curlObject, CURLOPT_USERPWD, userpwd.c_str());
            if (res != CURLE_OK) {
                return res;
            }
        }
        res = buildHeader(curlObject, options);
    }
    return res;
}
//=============================================================================
static CURLcode
setCurlURL(CURL* curlObject, std::string url)
{
    CURLcode res = CURLE_FAILED_INIT;
    if (curlObject != nullptr) {
        res = curl_easy_setopt(curlObject, CURLOPT_URL, url.c_str());
    }
    return res;
}
//=============================================================================
static size_t
write_data(void* ptr, size_t size, size_t nmemb, void* stream)
{
    size_t written = fwrite(ptr, size, nmemb, (FILE*)stream);
    if (NelsonConfiguration::getInstance()->getInterruptPending()) {
        return 0;
    }
    if (bHaveEventsLoop) {
        ProcessEventsDynamicFunction(false);
    }
    return written;
}
//=============================================================================
static std::string
scalarValueToString(CURL* curlObject, ArrayOf& value)
{
    std::string s;
    switch (value.getDataClass()) {
    case NLS_LOGICAL: {
        logical v = value.getContentAsLogicalScalar();
        s = (v ? "1" : "0");
    } break;
    case NLS_CHAR:
    case NLS_STRING_ARRAY: {
        s = value.getContentAsCString();
    } break;
    case NLS_DOUBLE: {
        double v = value.getContentAsDoubleScalar();
        if (std::floor(v) == std::ceil(v)) {
            s = std::to_string(int64(v));
        } else {
            s = std::to_string(v);
        }
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> v = value.getContentAsDoubleComplexScalar();
        if (std::floor(v.real()) == std::ceil(v.real())) {
            s = std::to_string(int64(v.real()));
        } else {
            s = std::to_string(v.real());
        }
        if (v.imag() > 0) {
            s += "+";
        }
        if (std::floor(v.imag()) == std::ceil(v.imag())) {
            s += std::to_string(int64(v.imag()));
        } else {
            s += std::to_string(v.imag());
        }
        s += "i";
    } break;
    case NLS_SINGLE: {
        single v = value.getContentAsSingleScalar();
        if (std::floor(v) == std::ceil(v)) {
            s = std::to_string(int64(v));
        } else {
            s = std::to_string(v);
        }
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> v = value.getContentAsSingleComplexScalar();
        if (std::floor(v.real()) == std::ceil(v.real())) {
            s = std::to_string(int64(v.real()));
        } else {
            s = std::to_string(v.real());
        }
        if (v.imag() > 0) {
            s += "+";
        }
        if (std::floor(v.imag()) == std::ceil(v.imag())) {
            s += std::to_string(int64(v.imag()));
        } else {
            s += std::to_string(v.imag());
        }
        s += "i";
    } break;
    case NLS_UINT8: {
        uint8 v = value.getContentAsUnsignedInteger8Scalar();
        s = std::to_string(v);
    } break;
    case NLS_INT8: {
        int8 v = value.getContentAsInteger8Scalar();
        s = std::to_string(v);
    } break;
    case NLS_UINT16: {
        uint16 v = value.getContentAsUnsignedInteger16Scalar();
        s = std::to_string(v);
    } break;
    case NLS_INT16: {
        int16 v = value.getContentAsInteger16Scalar();
        s = std::to_string(v);
    } break;
    case NLS_UINT32: {
        uint32 v = value.getContentAsUnsignedInteger32Scalar();
        s = std::to_string(v);
    } break;
    case NLS_INT32: {
        int32 v = value.getContentAsInteger32Scalar();
        s = std::to_string(v);
    } break;
    case NLS_UINT64: {
        uint64 v = value.getContentAsUnsignedInt64Scalar();
        s = std::to_string(v);
    } break;
    case NLS_INT64: {
        int64 v = value.getContentAsInteger64Scalar();
        s = std::to_string(v);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return curl_easy_escape(curlObject, s.c_str(), s.size());
}
//=============================================================================
static std::string
convertToOutputFormat(CURL* curlObject, const std::string& name, indexType k, indexType nbElements,
    const std::wstring& arrayOutputFormat, const std::string& s)
{
    std::string output;
    if (arrayOutputFormat == L"csv") {
        std::string t;
        if (k == 0) {
            t = s;
        } else {
            t = "," + s;
        }
        output = output + t;
    } else if (arrayOutputFormat == L"json") {
        std::string t;
        if (k == 0) {
            std::string b = "[" + s;
            t = protectCharacters(b);
        } else {
            if (k == nbElements - 1) {
                std::string b = s + "]";
                t = "," + protectCharacters(b);
            } else {
                t = "," + protectCharacters(s);
            }
        }
        output = output + t;
    } else if (arrayOutputFormat == L"repeating") {
        std::string t;
        if (k == 0) {
            t = protectCharacters(name) + "=" + protectCharacters(s);
        } else {
            t = "&" + protectCharacters(name) + "=" + protectCharacters(s);
        }
        output = output + t;
    } else if (arrayOutputFormat == L"php") {
        std::string t;
        if (k == 0) {
            t = protectCharacters(name + "[]") + "=" + protectCharacters(s);
        } else {
            t = "&" + protectCharacters(name + "[]") + "=" + protectCharacters(s);
        }
        output = output + t;
    }
    return output;
}
//=============================================================================
static std::string
arrayValueToString(CURL* curlObject, const std::string& name, ArrayOf& value,
    const std::wstring& arrayOutputFormat)
{
    std::string output;
    if (arrayOutputFormat == L"csv" || arrayOutputFormat == L"json") {
        output = std::string(curl_easy_escape(curlObject, name.c_str(), name.size())) + "=";
    }
    Dimensions dims = value.getDimensions();
    indexType nbElements = dims.getElementCount();
    switch (value.getDataClass()) {
    case NLS_LOGICAL: {
        logical* ptr = (logical*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::string s = (ptr[k] ? "1" : "0");
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_CHAR: {
        output = name + "=" + value.getContentAsCString();
        output = std::string(curl_easy_escape(curlObject, output.c_str(), output.size()));
    } break;
    case NLS_STRING_ARRAY: {
        ArrayOf* ptr = (ArrayOf*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            ArrayOf v = ptr[k];
            std::string s;
            if (v.isCharacterArray()) {
                s = v.getContentAsCString();
            } else {
                s = "NaN";
            }
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_CELL_ARRAY: {
        ArrayOf* ptr = (ArrayOf*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            ArrayOf v = ptr[k];
            scalarValueToString(curlObject, v);
            output = output + arrayValueToString(curlObject, name, v, arrayOutputFormat);
        }
    } break;
    case NLS_DOUBLE: {
        double* ptr = (double*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            double v = ptr[k];
            std::string s;
            if (std::floor(v) == std::ceil(v)) {
                s = std::to_string(int64(v));
            } else {
                s = std::to_string(v);
            }
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_DCOMPLEX: {
        double* ptr = (double*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::complex<double> v(ptr[k], ptr[k + 1]);
            std::string s;
            if (std::floor(v.real()) == std::ceil(v.real())) {
                s = std::to_string(int64(v.real()));
            } else {
                s = std::to_string(v.real());
            }
            if (v.imag() > 0) {
                s += "+";
            }
            if (std::floor(v.imag()) == std::ceil(v.imag())) {
                s += std::to_string(int64(v.imag()));
            } else {
                s += std::to_string(v.imag());
            }
            s += "i";
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_SINGLE: {
        single* ptr = (single*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            single v = ptr[k];
            std::string s;
            if (std::floor(v) == std::ceil(v)) {
                s = std::to_string(int64(v));
            } else {
                s = std::to_string(v);
            }
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_SCOMPLEX: {
        single* ptr = (single*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::complex<single> v(ptr[k], ptr[k + 1]);
            std::string s;
            if (std::floor(v.real()) == std::ceil(v.real())) {
                s = std::to_string(int64(v.real()));
            } else {
                s = std::to_string(v.real());
            }
            if (v.imag() > 0) {
                s += "+";
            }
            if (std::floor(v.imag()) == std::ceil(v.imag())) {
                s += std::to_string(int64(v.imag()));
            } else {
                s += std::to_string(v.imag());
            }
            s += "i";
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT8: {
        uint8* ptr = (uint8*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint8 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int8 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT16: {
        uint16* ptr = (uint16*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint16 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT16: {
        int16* ptr = (int16*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int16 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT32: {
        uint32* ptr = (uint32*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint32 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT32: {
        int32* ptr = (int32*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int32 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT64: {
        uint64* ptr = (uint64*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint64 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT64: {
        int64* ptr = (int64*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int64 v = ptr[k];
            std::string s = std::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return output;
}
//=============================================================================
static std::string
arrayOfToString(CURL* curlObject, const std::string& name, ArrayOf& value, WebOptions& options)
{
    std::string strValue;
    if (value.isEmpty()) {
        strValue = name;
    } else if (value.isScalar() || (value.isRowVectorCharacterArray() || value.isStringArray())) {
        strValue = name + "=";
        strValue += scalarValueToString(curlObject, value);
    } else {
        strValue = arrayValueToString(curlObject, name, value, options.getArrayformat());
    }
    return strValue;
}
//=============================================================================
static std::string
encodeUrl(CURL* curlObject, const std::wstring& url, const ArrayOfVector& names,
    const ArrayOfVector& values, WebOptions& options)
{
    std::string urlEncoded(wstring_to_utf8(url));

    for (size_t k = 0; k < names.size(); ++k) {
        ArrayOf nameArrayOf = names[k];
        ArrayOf valueArrayOf = values[k];
        std::string name = nameArrayOf.getContentAsCString();
        name = curl_easy_escape(curlObject, name.c_str(), name.size());
        std::string value = arrayOfToString(curlObject, name, valueArrayOf, options);
        if (k == 0) {
            urlEncoded.append("?");
        } else {
            urlEncoded.append("&");
        }
        urlEncoded.append(value);
    }
    return urlEncoded;
}
//=============================================================================
static std::wstring
errorCodeToMessage(long response_code)
{
    std::wstring message;
    switch (response_code) {
    case 400:
        return _W("Bad Request (400)");
    case 401:
        return _W("Unauthorized (401)");
    case 402:
        return _W("Payment Required (402)");
    case 403:
        return _W("Forbidden (403)");
    case 404:
        return _W("Not Found (404)");
    case 405:
        return _W("Method Not Allowed (405)");
    case 406:
        return _W("Not Acceptable (406)");
    case 407:
        return _W("Proxy Authentication Required (407)");
    case 408:
        return _W("Request Timeout (408)");
    case 409:
        return _W("Conflict (409)");
    case 410:
        return _W("Gone (410)");
    case 411:
        return _W("Length Required (411)");
    case 412:
        return _W("Precondition Failed (412)");
    case 413:
        return _W("Payload Too Large (413)");
    case 414:
        return _W("URI Too Long (414)");
    case 415:
        return _W("Unsupported Media Type (415)");
    case 416:
        return _W("Range Not Satisfiable (416)");
    case 417:
        return _W("Expectation Failed (417)");
    case 418:
        return _W("I'm a teapot (418)");
    case 421:
        return _W("Misdirected Request (421)");
    case 422:
        return _W("Unprocessable Entity (422)");
    case 423:
        return _W("Locked (423)");
    case 424:
        return _W("Failed Dependency (424)");
    case 425:
        return _W("Too Early (425)");
    case 426:
        return _W("Upgrade Required (426)");
    case 428:
        return _W("Precondition Required (428)");
    case 429:
        return _W("Too Many Requests (429)");
    case 431:
        return _W("Request Header Fields Too Large (431)");
    case 451:
        return _W("Unavailable For Legal Reasons (451)");
    case 500:
        return _W("Internal Server Error (500)");
    case 501:
        return _W("Not Implemented (501)");
    case 502:
        return _W("Bad Gateway (502)");
    case 503:
        return _W("Service Unavailable (503)");
    case 504:
        return _W("Gateway Timeout (504)");
    case 505:
        return _W("HTTP Version Not Supported (505)");
    case 506:
        return _W("Variant Also Negotiates (506)");
    case 507:
        return _W("Insufficient Storage (507)");
    case 508:
        return _W("Loop Detected (508)");
    case 510:
        return _W("Not Extended(510)");
    case 511:
        return _W("Network Authentication Required (511)");
    default:{
        if (response_code >= 400 && response_code < 500) {
            _W("HTTP Client error code: ") + std::to_wstring(response_code);
        } 
        if (response_code >= 500 && response_code < 600) {
            _W("HTTP Server error code: ") + std::to_wstring(response_code);
        } 
    } break;
    }
    return message;
}
//=============================================================================
ArrayOf
WebSave(const std::wstring& url, const std::wstring& filename, const ArrayOfVector& names,
    const ArrayOfVector& values, WebOptions& options, bool haveEventsLoop)
{
    bHaveEventsLoop = haveEventsLoop;
    FILE* fw;
    std::wstring fullFilename;
#ifdef _MSC_VER
    fw = _wfopen(filename.c_str(), L"wb");
#else
    std::string utfFilename = wstring_to_utf8(filename);
    fw = fopen(utfFilename.c_str(), "wb");
#endif
    if (!fw) {
        Error(_W("Cannot create destination file."));
    }
    boost::filesystem::path p(filename);
    try {
        p = boost::filesystem::absolute(p);
        fullFilename = p.generic_wstring();
    } catch (const boost::filesystem::filesystem_error&) {
        fullFilename = p.generic_wstring();
    }
    CURL* curlObject = curl_easy_init();
    if (curlObject == nullptr) {
        fclose(fw);
        Error(_W("Cannot initialize websave."));
    }
    CURLcode curlCode = setCurlWebOptions(curlObject, options);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }
    curlCode = setCurlURL(curlObject, encodeUrl(curlObject, url, names, values, options));
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    if (options.getRequestMethod() == L"auto" || options.getRequestMethod() == L"get") {

    } else if (options.getRequestMethod() == L"delete") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "DELETE");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    } else if (options.getRequestMethod() == L"put") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "PUT");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    } else if (options.getRequestMethod() == L"patch") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "PATCH");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    } else if (options.getRequestMethod() == L"post") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    }

    std::wstring certificateFilename = options.getCertificateFilename();
    if (!certificateFilename.empty()) {
        curlCode = curl_easy_setopt(
            curlObject, CURLOPT_CAINFO, wstring_to_utf8(certificateFilename).c_str());
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
        curlCode = curl_easy_setopt(curlObject, CURLOPT_SSL_VERIFYPEER, 1L);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    } else {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_SSL_VERIFYPEER, 0L);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    }

    curlCode = curl_easy_setopt(curlObject, CURLOPT_WRITEFUNCTION, write_data);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }
    curlCode = curl_easy_setopt(curlObject, CURLOPT_WRITEDATA, fw);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    double timeout = options.getTimeout();
    if (!std::isinf(timeout)) {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CONNECTTIMEOUT_MS, (long)timeout * 1000);
    } else {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CONNECTTIMEOUT_MS, (long)0);
        if (curlCode == CURLE_OK) {
            curlCode = curl_easy_setopt(curlObject, CURLOPT_NOSIGNAL, 1);
        }
    }
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    curlCode = curl_easy_setopt(curlObject, CURLOPT_FOLLOWLOCATION, 1L);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    curlCode = curl_easy_perform(curlObject);
    fclose(fw);
    if (curlCode != CURLE_OK) {
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    long response_code;
    curlCode = curl_easy_getinfo(curlObject, CURLINFO_RESPONSE_CODE, &response_code);
    if (curlCode != CURLE_OK) {
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    curl_easy_cleanup(curlObject);
    std::wstring msg = errorCodeToMessage(response_code);
    if (!msg.empty()) {
        // remove file if error detected.
        try {
            boost::filesystem::path p = filename;
            boost::filesystem::permissions(p,
                boost::filesystem::add_perms | boost::filesystem::owner_write
                    | boost::filesystem::group_write | boost::filesystem::others_write);

            boost::filesystem::remove(p);
        } catch (const boost::filesystem::filesystem_error&) {
        }
        Error(msg);
    }
    return ArrayOf::characterArrayConstructor(fullFilename);
}
//=============================================================================
static void
initGuiDynamicLibrary()
{
    if (bFirstDynamicLibraryCall) {
        std::string fullpathGuiSharedLibrary
            = "libnlsGui" + Nelson::get_dynamic_library_extension();
#ifdef _MSC_VER
        char* buf;
        try {
            buf = new char[MAX_PATH];
        } catch (const std::bad_alloc&) {
            buf = nullptr;
        }
        if (buf != nullptr) {
            DWORD dwRet = ::GetEnvironmentVariableA("NELSON_BINARY_PATH", buf, MAX_PATH);
            if (dwRet != 0U) {
                fullpathGuiSharedLibrary
                    = std::string(buf) + std::string("/") + fullpathGuiSharedLibrary;
            }
            delete[] buf;
        }
#else
        char const* tmp = getenv("NELSON_BINARY_PATH");
        if (tmp != nullptr) {
            fullpathGuiSharedLibrary
                = std::string(tmp) + std::string("/") + fullpathGuiSharedLibrary;
        }
#endif
        nlsGuiHandleDynamicLibrary = Nelson::load_dynamic_library(fullpathGuiSharedLibrary);
        if (nlsGuiHandleDynamicLibrary != nullptr) {
            bFirstDynamicLibraryCall = false;
        }
    }
}
//=============================================================================
static void
ProcessEventsDynamicFunction(bool bWait)
{
    using PROC_ProcessEvents = void (*)(bool);
    static PROC_ProcessEvents ProcessEventsPtr = nullptr;
    initGuiDynamicLibrary();
    if (ProcessEventsPtr == nullptr) {
        ProcessEventsPtr = reinterpret_cast<PROC_ProcessEvents>(
            Nelson::get_function(nlsGuiHandleDynamicLibrary, "NelSonProcessEvents"));
    }
    if (ProcessEventsPtr != nullptr) {
        ProcessEventsPtr(bWait);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
