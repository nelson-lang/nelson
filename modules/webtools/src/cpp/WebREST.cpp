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
#include <fmt/xchar.h>
#include <curl/curl.h>
#include "WebREST.hpp"
#include "WebParameters.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "ResponseCodeToMessage.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "RuntimeDispatch.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool bHaveEventsLoop = false;
//=============================================================================
struct WriteData
{
    FILE* fw;
    size_t ID;
};
//=============================================================================
// Frees the curl header list on every return path and on exceptions from Error().
struct CurlSListGuard
{
    struct curl_slist* chunk = nullptr;
    CurlSListGuard() = default;
    CurlSListGuard(const CurlSListGuard&) = delete;
    CurlSListGuard&
    operator=(const CurlSListGuard&)
        = delete;
    ~CurlSListGuard()
    {
        if (chunk != nullptr) {
            curl_slist_free_all(chunk);
        }
    }
};
//=============================================================================
static CURLcode
buildHeader(CURL* curlObject, const WebOptions& options, struct curl_slist** chunkOut);
static CURLcode
setCurlWebOptions(CURL* curlObject, const WebOptions& options, struct curl_slist** chunkOut);
static CURLcode
setCurlCertificateOptions(CURL* curlObject, const WebOptions& options);
static CURLcode
setCurlURL(CURL* curlObject, const std::string& url);
static size_t
write_data(void* ptr, size_t size, size_t nmemb, void* stream);
//=============================================================================
std::wstring
WebREST(const std::wstring& url, const std::wstring& data, std::wstring& filename,
    const stringVector& names, const ArrayOfVector& values, WebOptions& options,
    bool haveEventsLoop, size_t evaluatorID, std::wstring& contentType)
{
    contentType.clear();
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
        Error(L"Nelson:webrest:cannotCreateFile", L"Cannot create destination file.");
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
            Error(L"Nelson:webrest:initFailed", L"Cannot initialize %s.", L"webwrite");
        } else {
            Error(L"Nelson:webrest:initFailed", L"Cannot initialize %s.", L"websave");
        }
    }
    CurlSListGuard headerGuard;
    CURLcode curlCode = setCurlWebOptions(curlObject, options, &headerGuard.chunk);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }
    if (isWrite) {
        curlCode = setCurlURL(curlObject, wstring_to_utf8(url));
    } else {
        curlCode = setCurlURL(curlObject, encodeUrl(url, names, values, options));
    }
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    if (options.getRequestMethod() == L"auto") {
        if (isWrite) {
            curlCode = curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
            if (curlCode != CURLE_OK) {
                fclose(fw);
                std::string msg = curl_easy_strerror(curlCode);
                curl_easy_cleanup(curlObject);
                Error("Nelson:webREST:curlError", msg);
            }
        }
    } else if (options.getRequestMethod() == L"post") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_POST, 1L);
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error("Nelson:webREST:curlError", msg);
        }
    } else if (options.getRequestMethod() == L"get") {
    } else if (options.getRequestMethod() == L"delete") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "DELETE");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error("Nelson:webREST:curlError", msg);
        }
    } else if (options.getRequestMethod() == L"put") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "PUT");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error("Nelson:webREST:curlError", msg);
        }
    } else if (options.getRequestMethod() == L"patch") {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CUSTOMREQUEST, "PATCH");
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error("Nelson:webREST:curlError", msg);
        }
    }

    curlCode = curl_easy_setopt(
        curlObject, CURLOPT_FOLLOWLOCATION, options.getFollowLocation() ? 1L : 0L);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    // An empty string enables every codec curl was built with and decompresses
    // the body, so the file receives the decompressed bytes.
    curlCode = curl_easy_setopt(curlObject, CURLOPT_ACCEPT_ENCODING, "");
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    curlCode = setCurlCertificateOptions(curlObject, options);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    curlCode = curl_easy_setopt(curlObject, CURLOPT_WRITEFUNCTION, write_data);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }
    WriteData writeData;
    writeData.fw = fw;
    writeData.ID = evaluatorID;
    curlCode = curl_easy_setopt(curlObject, CURLOPT_WRITEDATA, &writeData);
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    // Timeout bounds the wait for the server (connection, then any stall of the
    // transfer), not the whole download: a large file on a slow link must not
    // fail after 5 seconds.
    double timeout = options.getTimeout();
    if (std::isinf(timeout)) {
        curlCode = curl_easy_setopt(curlObject, CURLOPT_NOSIGNAL, 1);
    } else {
        long ltimeout = (long)(timeout * 1000);
        curlCode = curl_easy_setopt(curlObject, CURLOPT_CONNECTTIMEOUT_MS, ltimeout);
        if (curlCode == CURLE_OK) {
            curlCode = curl_easy_setopt(curlObject, CURLOPT_LOW_SPEED_LIMIT, 1L);
        }
        if (curlCode == CURLE_OK) {
            long stallSeconds = (long)std::ceil(timeout);
            curlCode = curl_easy_setopt(
                curlObject, CURLOPT_LOW_SPEED_TIME, stallSeconds > 0 ? stallSeconds : 1L);
        }
    }
    if (curlCode != CURLE_OK) {
        fclose(fw);
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    std::string utfData;
    if (isWrite) {
        if (data == L"__WEBWRITE__") {
            utfData = formEncode(names, values, options);
        } else {
            utfData = wstring_to_utf8(data);
        }
        curlCode = curl_easy_setopt(curlObject, CURLOPT_POSTFIELDS, utfData.c_str());
        if (curlCode != CURLE_OK) {
            fclose(fw);
            std::string msg = curl_easy_strerror(curlCode);
            curl_easy_cleanup(curlObject);
            Error("Nelson:webREST:curlError", msg);
        }
    }

    curlCode = curl_easy_perform(curlObject);
    fclose(fw);
    if (curlCode != CURLE_OK) {
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    long response_code;
    curlCode = curl_easy_getinfo(curlObject, CURLINFO_RESPONSE_CODE, &response_code);
    if (curlCode != CURLE_OK) {
        std::string msg = curl_easy_strerror(curlCode);
        curl_easy_cleanup(curlObject);
        Error("Nelson:webREST:curlError", msg);
    }

    char* responseContentType = nullptr;
    curlCode = curl_easy_getinfo(curlObject, CURLINFO_CONTENT_TYPE, &responseContentType);
    if (curlCode == CURLE_OK && responseContentType != nullptr) {
        contentType = utf8_to_wstring(responseContentType);
    }

    curl_easy_cleanup(curlObject);
    std::wstring msg = responseCodeToMessage(response_code);
    if (!msg.empty()) {
        FileSystemWrapper::Path _p = filename;
        FileSystemWrapper::Path::remove(_p);
        Error(L"Nelson:webREST:httpError", msg);
    }
    return fullFilename;
}
//=============================================================================
static CURLcode
buildHeader(CURL* curlObject, const WebOptions& options, struct curl_slist** chunkOut)
{
    struct curl_slist* chunk = nullptr;
    for (const std::string& line : buildHeaderLines(options)) {
        chunk = curl_slist_append(chunk, line.c_str());
    }
    *chunkOut = chunk;
    return curl_easy_setopt(curlObject, CURLOPT_HTTPHEADER, chunk);
}
static CURLcode
setCurlWebOptions(CURL* curlObject, const WebOptions& options, struct curl_slist** chunkOut)
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
        res = buildHeader(curlObject, options, chunkOut);
    }
    return res;
}
//=============================================================================
static std::wstring
getDefaultCertificateFilename()
{
    FileSystemWrapper::Path p(NelsonConfiguration::getInstance()->getNelsonModulesDirectory());
    p /= L"webtools";
    p /= L"resources";
    p /= L"cacert.pem";
    if (p.is_regular_file()) {
        return p.generic_wstring();
    }
    return L"";
}
//=============================================================================
static CURLcode
setCurlCertificateOptions(CURL* curlObject, const WebOptions& options)
{
    std::wstring certificateFilename = options.getCertificateFilename();
    bool useDefaultCertificate = (certificateFilename == L"default");
    if (certificateFilename == L"default") {
        certificateFilename = getDefaultCertificateFilename();
    }

    CURLcode res = CURLE_FAILED_INIT;
    if (certificateFilename.empty()) {
        res = curl_easy_setopt(curlObject, CURLOPT_SSL_VERIFYPEER, useDefaultCertificate ? 1L : 0L);
        return res;
    }

    std::string utfCertificateFilename = wstring_to_utf8(certificateFilename);
    res = curl_easy_setopt(curlObject, CURLOPT_CAINFO, utfCertificateFilename.c_str());
    if (res != CURLE_OK) {
        return res;
    }
    return curl_easy_setopt(curlObject, CURLOPT_SSL_VERIFYPEER, 1L);
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
        Nelson::processGuiEvents(false);
    }
    return written;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
