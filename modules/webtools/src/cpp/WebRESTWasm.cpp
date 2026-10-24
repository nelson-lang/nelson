//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// webread / websave / webwrite for the WebAssembly engine.
//
// The engine runs inside a dedicated Web Worker, where a synchronous
// XMLHttpRequest is allowed: the request blocks the interpreter exactly like
// the libcurl backend does natively, so no asynchronous machinery is needed.
// The Worker installs Module['onNelsonWebRequest'] (tools/wasm/nelson-web-request.mjs);
// without it (Node.js test runner) web access is reported as unavailable.
//
// Differences with the native backend, imposed by the browser: the server must
// allow the page's origin (CORS), the browser sets the User-Agent itself,
// redirects are always followed (FollowLocation is ignored), certificates are
// the browser's and a running request cannot be interrupted.
//=============================================================================
#include <cctype>
#include <cmath>
#include <cstdio>
#include <vector>
#include <emscripten/emscripten.h>
#include "nlohmann/json.hpp"
#include "WebREST.hpp"
#include "WebParameters.hpp"
#include "ResponseCodeToMessage.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // Status codes below zero are failures reported by the JavaScript side.
    constexpr int WEB_REQUEST_UNAVAILABLE = -1;
    constexpr int WEB_REQUEST_TIMEOUT = -2;
    constexpr int WEB_REQUEST_NETWORK_ERROR = -3;
    //=============================================================================
    std::string
    requestMethod(const WebOptions& options, bool isWrite)
    {
        const std::wstring method = options.getRequestMethod();
        if (method == L"auto") {
            return isWrite ? "POST" : "GET";
        }
        std::string upper = wstring_to_utf8(method);
        for (char& c : upper) {
            c = static_cast<char>(toupper(static_cast<unsigned char>(c)));
        }
        return upper;
    }
    //=============================================================================
    std::string
    requestSpecification(const std::string& url, const std::string& method, const std::string& body,
        bool isWrite, const WebOptions& options)
    {
        nlohmann::json headers = nlohmann::json::array();
        for (const std::string& line : buildHeaderLines(options)) {
            const size_t colon = line.find(':');
            if (colon == std::string::npos) {
                continue;
            }
            // "charsets" is a webtools convention, not an HTTP header: in a
            // browser any non-standard header forces a CORS preflight, which
            // servers that otherwise allow the request would reject.
            if (line.compare(0, colon, "charsets") == 0) {
                continue;
            }
            std::string value = line.substr(colon + 1);
            const size_t start = value.find_first_not_of(' ');
            value = (start == std::string::npos) ? std::string() : value.substr(start);
            headers.push_back({ line.substr(0, colon), value });
        }
        const double timeout = options.getTimeout();
        nlohmann::json spec = { { "url", url }, { "method", method }, { "headers", headers },
            { "timeoutMs", std::isinf(timeout) ? 0.0 : std::floor(timeout * 1000.0) },
            { "username", wstring_to_utf8(options.getUsername()) },
            { "password", wstring_to_utf8(options.getPassword()) } };
        if (isWrite) {
            spec["body"] = body;
        }
        return spec.dump();
    }
    //=============================================================================
    // Runs the request on the JavaScript side. Returns the HTTP status, or a
    // negative code; the response body (or the failure text) is parked in
    // Module['__nelsonWebResponse'] until collected below.
    int
    performRequest(const std::string& spec)
    {
        // clang-format off
        return EM_ASM_INT(
            {
                const perform = Module['onNelsonWebRequest'];
                if (typeof perform != 'function') {
                    return -1;
                }
                let result;
                try {
                    result = perform(JSON.parse(UTF8ToString($0)));
                } catch (error) {
                    Module['__nelsonWebResponse'] = String(error && error.message ? error.message : error);
                    return -3;
                }
                if (!result || typeof result != 'object') {
                    return -3;
                }
                if (result.error) {
                    Module['__nelsonWebResponse'] = String(result.error);
                    return result.kind == 'timeout' ? -2 : (result.kind == 'unavailable' ? -1 : -3);
                }
                Module['__nelsonWebResponse'] = result.body || new Uint8Array(0);
                Module['__nelsonWebContentType'] = result.contentType || '';
                return Number(result.status) | 0;
            },
            spec.c_str());
        // clang-format on
    }
    //=============================================================================
    size_t
    responseLength()
    {
        // clang-format off
        return static_cast<size_t>(EM_ASM_INT({
            const response = Module['__nelsonWebResponse'];
            if (typeof response == 'string') {
                return lengthBytesUTF8(response);
            }
            return response ? response.byteLength : 0;
        }));
        // clang-format on
    }
    //=============================================================================
    std::vector<char>
    takeResponse()
    {
        std::vector<char> bytes(responseLength() + 1);
        // clang-format off
        EM_ASM(
            {
                const response = Module['__nelsonWebResponse'];
                delete Module['__nelsonWebResponse'];
                if (typeof response == 'string') {
                    stringToUTF8(response, $0, $1);
                } else if (response) {
                    HEAPU8.set(response, $0);
                }
            },
            bytes.data(), bytes.size());
        // clang-format on
        bytes.pop_back();
        return bytes;
    }
    //=============================================================================
    std::string
    takeContentType()
    {
        std::string ct(256, '\0');
        // clang-format off
        int len = EM_ASM_INT(
            {
                const ct = Module['__nelsonWebContentType'] || '';
                delete Module['__nelsonWebContentType'];
                const n = Math.min(lengthBytesUTF8(ct), $1 - 1);
                stringToUTF8(ct, $0, n + 1);
                return n;
            },
            ct.data(), static_cast<int>(ct.size()));
        // clang-format on
        ct.resize(static_cast<size_t>(len < 0 ? 0 : len));
        return ct;
    }
    //=============================================================================
    void
    raiseFailure(int code, const std::wstring& filename)
    {
        FileSystemWrapper::Path::remove(FileSystemWrapper::Path(filename));
        std::vector<char> detail = takeResponse();
        std::string text(detail.begin(), detail.end());
        while (!text.empty() && (text.back() == '.' || text.back() == ' ')) {
            text.pop_back();
        }
        switch (code) {
        case WEB_REQUEST_UNAVAILABLE:
            Error(L"Nelson:webREST:unavailable",
                L"Web access is unavailable in this runtime (no browser network stack).");
            break;
        case WEB_REQUEST_TIMEOUT:
            Error(L"Nelson:webREST:timeout", L"Timeout was reached");
            break;
        default:
            Error(L"Nelson:webREST:networkError",
                L"The request failed: %s. The server is unreachable, or it does not allow this "
                L"page's origin (CORS).",
                utf8_to_wstring(text.empty() ? std::string("network error") : text));
            break;
        }
    }
    //=============================================================================
} // namespace
//=============================================================================
std::wstring
WebREST(const std::wstring& url, const std::wstring& data, std::wstring& filename,
    const stringVector& names, const ArrayOfVector& values, WebOptions& options,
    bool /*haveEventsLoop*/, size_t /*evaluatorID*/, std::wstring& contentType)
{
    contentType.clear();
    const bool isWrite = !data.empty();
    const std::string utfFilename = wstring_to_utf8(filename);
    FILE* fw = fopen(utfFilename.c_str(), "wb");
    if (fw == nullptr) {
        Error(L"Nelson:webrest:cannotCreateFile", L"Cannot create destination file.");
    }
    fclose(fw);
    FileSystemWrapper::Path p(filename);
    std::string errorMessage;
    p = FileSystemWrapper::Path::absolute(p, errorMessage);
    if (!errorMessage.empty()) {
        p = filename;
    }
    const std::wstring fullFilename = p.generic_wstring();

    std::string body;
    std::string requestUrl;
    if (isWrite) {
        body = (data == L"__WEBWRITE__") ? formEncode(names, values, options)
                                         : wstring_to_utf8(data);
        requestUrl = wstring_to_utf8(url);
    } else {
        requestUrl = encodeUrl(url, names, values, options);
    }
    const std::string spec
        = requestSpecification(requestUrl, requestMethod(options, isWrite), body, isWrite, options);

    const int status = performRequest(spec);
    if (status < 0) {
        raiseFailure(status, filename);
    }
    std::vector<char> bytes = takeResponse();
    contentType = utf8_to_wstring(takeContentType());
    fw = fopen(utfFilename.c_str(), "wb");
    if (fw == nullptr) {
        Error(L"Nelson:webrest:cannotCreateFile", L"Cannot create destination file.");
    }
    const size_t written = bytes.empty() ? 0 : fwrite(bytes.data(), 1, bytes.size(), fw);
    fclose(fw);
    if (written != bytes.size()) {
        FileSystemWrapper::Path::remove(FileSystemWrapper::Path(filename));
        Error(L"Nelson:webrest:cannotCreateFile", L"Cannot create destination file.");
    }
    const std::wstring msg = responseCodeToMessage(status);
    if (!msg.empty()) {
        FileSystemWrapper::Path::remove(FileSystemWrapper::Path(filename));
        Error(L"Nelson:webREST:httpError", msg);
    }
    return fullFilename;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
