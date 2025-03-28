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
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <curl/curl.h>
#include <unordered_map>
#include "StringHelpers.hpp"
#include "WebREST.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "ResponseCodeToMessage.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ProcessEventsDynamic.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool bHaveEventsLoop = false;
static std::unordered_map<std::string, std::string> unsafeCharacters;
//=============================================================================
struct WriteData
{
    FILE* fw;
    size_t ID;
};
//=============================================================================
static std::string
protectCharacters(const std::string& str);
static CURLcode
buildHeader(CURL* curlObject, const WebOptions& options);
static CURLcode
setCurlWebOptions(CURL* curlObject, const WebOptions& options);
static CURLcode
setCurlURL(CURL* curlObject, const std::string& url);
static size_t
write_data(void* ptr, size_t size, size_t nmemb, void* stream);
static std::string
scalarValueToString(CURL* curlObject, ArrayOf& value);
static std::string
convertToOutputFormat(CURL* curlObject, const std::string& name, indexType k, indexType nbElements,
    const std::wstring& arrayOutputFormat, const std::string& s);
static std::string
arrayOfToString(CURL* curlObject, const std::string& name, ArrayOf& value, WebOptions& options);
static std::string
encodeUrl(CURL* curlObject, const std::wstring& url, const stringVector& names,
    const ArrayOfVector& values, WebOptions& options);
static std::string
formEncode(
    CURL* curlObject, const stringVector& names, const ArrayOfVector& values, WebOptions& options);
//=============================================================================
std::wstring
WebREST(const std::wstring& url, const std::wstring& data, std::wstring& filename,
    const stringVector& names, const ArrayOfVector& values, WebOptions& options,
    bool haveEventsLoop, size_t evaluatorID)
{
    bool isWrite = !data.empty();
    std::wstring fullFilename;
    FILE* fw;
#ifdef _MSC_VER
    fw = _wfopen(filename.c_str(), L"wb");
#else
    std::string utfFilename = wstring_to_utf8(filename);
    fw = fopen(utfFilename.c_str(), "wb");
#endif
    if (fw == nullptr) {
        Error(_W("Cannot create destination file."));
    }
    FileSystemWrapper::Path p(filename);
    std::string errorMessage;
    p = FileSystemWrapper::Path::absolute(p, errorMessage);
    if (!errorMessage.empty()) {
        p = filename;
    }
    fullFilename = p.generic_wstring();
    CURL* curlObject = curl_easy_init();
    if (curlObject == nullptr) {
        fclose(fw);
        if (isWrite) {
            Error(_W("Cannot initialize webwrite."));
        } else {
            Error(_W("Cannot initialize websave."));
        }
    }
    CURLcode curlCode = setCurlWebOptions(curlObject, options);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }
    if (isWrite) {
        curlCode = setCurlURL(curlObject, wstring_to_utf8(url));
    } else {
        curlCode = setCurlURL(curlObject, encodeUrl(curlObject, url, names, values, options));
    }
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    if (options.getRequestMethod() == L"auto") {
        if (isWrite) {
            curlCode = curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
            if (curlCode != CURLE_OK) {
                fclose(fw);
                std::string msg = curl_easy_strerror(curlCode);
                curl_easy_cleanup(curlObject);
                Error(msg);
            }
        }
    } else if (options.getRequestMethod() == L"post") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    } else if (options.getRequestMethod() == L"get") {
        // NOTHING TO DO
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
    }

    if (options.getFollowLocation()) {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_FOLLOWLOCATION, 1L);
    } else {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_FOLLOWLOCATION, 0L);
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
    WriteData writeData;
    writeData.fw = fw;
    writeData.ID = evaluatorID;
    curlCode = curl_easy_setopt(curlObject, CURLOPT_WRITEDATA, &writeData);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error(msg);
    }

    double timeout = options.getTimeout();
    long ltimeout = 0;
    if (!std::isinf(timeout)) {
        ltimeout = (long)(timeout * 1000);
    } else {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_NOSIGNAL, 1);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
    }

    curlCode = curl_easy_setopt(curlObject, CURLOPT_TIMEOUT_MS, ltimeout);
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
    std::string utfData;
    if (isWrite) {
        if (data == L"__WEBWRITE__") {
            utfData = formEncode(curlObject, names, values, options);
        } else {
            utfData = wstring_to_utf8(data);
        }
        curlCode = curl_easy_setopt(curlObject, CURLOPT_POSTFIELDS, utfData.c_str());
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error(msg);
        }
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
    std::wstring msg = responseCodeToMessage(response_code);
    if (!msg.empty()) {
        // remove file if error detected.
        FileSystemWrapper::Path _p = filename;
        FileSystemWrapper::Path::remove(_p);
        Error(msg);
    }
    return fullFilename;
}
//=============================================================================
static std::string
protectCharacters(const std::string& str)
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
        std::unordered_map<std::string, std::string>::const_iterator found
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
static CURLcode
buildHeader(CURL* curlObject, const WebOptions& options)
{
    stringVector lines;
    lines.reserve(7);
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
        }
    }

    if (!options.getMediaType().empty()) {
        lines.push_back("Content-Type: " + wstring_to_utf8(options.getMediaType()));
    }

    struct curl_slist* chunk = nullptr;
    for (const std::string& l : lines) {
        chunk = curl_slist_append(chunk, l.c_str());
    }
    wstringVector headerFields = options.getHeaderFields();
    size_t nbFields = headerFields.size();
    if (nbFields > 0) {
        if (nbFields % 2) {
            Error(_W("Invalid HeaderFields size."));
        }
        size_t nbFieldnames = nbFields / 2;
        for (size_t k = 0; k < nbFieldnames; k++) {
            std::string header = wstring_to_utf8(headerFields[k]) + ": "
                + wstring_to_utf8(headerFields[k + nbFieldnames]);
            chunk = curl_slist_append(chunk, header.c_str());
        }
    }

    return curl_easy_setopt(curlObject, CURLOPT_HTTPHEADER, chunk);
}
//=============================================================================
static CURLcode
setCurlWebOptions(CURL* curlObject, const WebOptions& options)
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
setCurlURL(CURL* curlObject, const std::string& url)
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
    WriteData* writeData = static_cast<WriteData*>(stream);
    FILE* fw = writeData->fw;
    size_t ID = writeData->ID;
    size_t written = fwrite(ptr, size, nmemb, fw);
    if (NelsonConfiguration::getInstance()->getInterruptPending(ID)) {
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
            s = fmt::to_string(int64(v));
        } else {
            s = fmt::to_string(v);
        }
    } break;
    case NLS_DCOMPLEX: {
        std::complex<double> v = value.getContentAsDoubleComplexScalar();
        if (std::floor(v.real()) == std::ceil(v.real())) {
            s = fmt::to_string(int64(v.real()));
        } else {
            s = fmt::to_string(v.real());
        }
        if (v.imag() > 0) {
            s += "+";
        }
        if (std::floor(v.imag()) == std::ceil(v.imag())) {
            s += fmt::to_string(int64(v.imag()));
        } else {
            s += fmt::to_string(v.imag());
        }
        s += "i";
    } break;
    case NLS_SINGLE: {
        single v = value.getContentAsSingleScalar();
        if (std::floor(v) == std::ceil(v)) {
            s = fmt::to_string(int64(v));
        } else {
            s = fmt::to_string(v);
        }
    } break;
    case NLS_SCOMPLEX: {
        std::complex<single> v = value.getContentAsSingleComplexScalar();
        if (std::floor(v.real()) == std::ceil(v.real())) {
            s = fmt::to_string(int64(v.real()));
        } else {
            s = fmt::to_string(v.real());
        }
        if (v.imag() > 0) {
            s += "+";
        }
        if (std::floor(v.imag()) == std::ceil(v.imag())) {
            s += fmt::to_string(int64(v.imag()));
        } else {
            s += fmt::to_string(v.imag());
        }
        s += "i";
    } break;
    case NLS_UINT8: {
        uint8 v = value.getContentAsUnsignedInteger8Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_INT8: {
        int8 v = value.getContentAsInteger8Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_UINT16: {
        uint16 v = value.getContentAsUnsignedInteger16Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_INT16: {
        int16 v = value.getContentAsInteger16Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_UINT32: {
        uint32 v = value.getContentAsUnsignedInteger32Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_INT32: {
        int32 v = value.getContentAsInteger32Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_UINT64: {
        uint64 v = value.getContentAsUnsignedInteger64Scalar();
        s = fmt::to_string(v);
    } break;
    case NLS_INT64: {
        int64 v = value.getContentAsInteger64Scalar();
        s = fmt::to_string(v);
    } break;
    default: {
        Error(_W("Type not managed."));
    } break;
    }
    return curl_easy_escape(curlObject, s.c_str(), (int)s.size());
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
        output = std::string(curl_easy_escape(curlObject, name.c_str(), (int)name.size())) + "=";
    }
    Dimensions dims = value.getDimensions();
    indexType nbElements = dims.getElementCount();
    switch (value.getDataClass()) {
    case NLS_LOGICAL: {
        auto* ptr = (logical*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::string s = (ptr[k] ? "1" : "0");
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_CHAR: {
        output = name + "=" + value.getContentAsCString();
        output = std::string(curl_easy_escape(curlObject, output.c_str(), (int)output.size()));
    } break;
    case NLS_STRING_ARRAY: {
        auto* ptr = (ArrayOf*)value.getDataPointer();
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
        auto* ptr = (ArrayOf*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            ArrayOf v = ptr[k];
            scalarValueToString(curlObject, v);
            output = output + arrayValueToString(curlObject, name, v, arrayOutputFormat);
        }
    } break;
    case NLS_DOUBLE: {
        auto* ptr = (double*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            double v = ptr[k];
            std::string s;
            if (std::floor(v) == std::ceil(v)) {
                s = fmt::to_string(int64(v));
            } else {
                s = fmt::to_string(v);
            }
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_DCOMPLEX: {
        auto* ptr = (double*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::complex<double> v(ptr[k], ptr[k + 1]);
            std::string s;
            if (std::floor(v.real()) == std::ceil(v.real())) {
                s = fmt::to_string(int64(v.real()));
            } else {
                s = fmt::to_string(v.real());
            }
            if (v.imag() > 0) {
                s += "+";
            }
            if (std::floor(v.imag()) == std::ceil(v.imag())) {
                s += fmt::to_string(int64(v.imag()));
            } else {
                s += fmt::to_string(v.imag());
            }
            s += "i";
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_SINGLE: {
        auto* ptr = (single*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            single v = ptr[k];
            std::string s;
            if (std::floor(v) == std::ceil(v)) {
                s = fmt::to_string(int64(v));
            } else {
                s = fmt::to_string(v);
            }
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_SCOMPLEX: {
        auto* ptr = (single*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            std::complex<single> v(ptr[k], ptr[k + 1]);
            std::string s;
            if (std::floor(v.real()) == std::ceil(v.real())) {
                s = fmt::to_string(int64(v.real()));
            } else {
                s = fmt::to_string(v.real());
            }
            if (v.imag() > 0) {
                s += "+";
            }
            if (std::floor(v.imag()) == std::ceil(v.imag())) {
                s += fmt::to_string(int64(v.imag()));
            } else {
                s += fmt::to_string(v.imag());
            }
            s += "i";
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT8: {
        auto* ptr = (uint8*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint8 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT8: {
        int8* ptr = (int8*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int8 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT16: {
        auto* ptr = (uint16*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint16 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT16: {
        auto* ptr = (int16*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int16 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT32: {
        auto* ptr = (uint32*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint32 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT32: {
        auto* ptr = (int32*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int32 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_UINT64: {
        auto* ptr = (uint64*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            uint64 v = ptr[k];
            std::string s = fmt::to_string(v);
            output = output
                + convertToOutputFormat(curlObject, name, k, nbElements, arrayOutputFormat, s);
        }
    } break;
    case NLS_INT64: {
        auto* ptr = (int64*)value.getDataPointer();
        for (indexType k = 0; k < nbElements; k++) {
            int64 v = ptr[k];
            std::string s = fmt::to_string(v);
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
formEncode(
    CURL* curlObject, const stringVector& names, const ArrayOfVector& values, WebOptions& options)
{
    std::string form;
    for (size_t k = 0; k < names.size(); ++k) {
        ArrayOf valueArrayOf = values[k];
        std::string name = names[k];
        name = curl_easy_escape(curlObject, name.c_str(), (int)name.size());
        std::string value = arrayOfToString(curlObject, name, valueArrayOf, options);
        if (k != 0) {
            form.append("&");
        }
        form.append(value);
    }
    return form;
}
//=============================================================================
static std::string
encodeUrl(CURL* curlObject, const std::wstring& url, const stringVector& names,
    const ArrayOfVector& values, WebOptions& options)
{
    std::string urlEncoded(wstring_to_utf8(url));
    std::string form = formEncode(curlObject, names, values, options);

    if (!StringHelpers::ends_with(urlEncoded, "?") && !form.empty()) {
        urlEncoded.append("?");
    }
    if (!form.empty()) {
        urlEncoded.append(form);
    }
    return urlEncoded;
}
//=============================================================================
}
//=============================================================================
