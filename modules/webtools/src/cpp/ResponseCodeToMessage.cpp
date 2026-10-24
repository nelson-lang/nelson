//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ResponseCodeToMessage.hpp"
#include "i18n.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
responseCodeToMessage(long response_code)
{
    switch (response_code) {
    case 400:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:badRequest", L"Bad Request (400)");
    case 401:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:unauthorized", L"Unauthorized (401)");
    case 402:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:paymentRequired", L"Payment Required (402)");
    case 403:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:forbidden", L"Forbidden (403)");
    case 404:
        return resolveMessageTemplate(L"Nelson:webtools:notFound", L"Not Found (404)");
    case 405:
        return resolveMessageTemplate(
            L"Nelson:webtools:methodAllowed", L"Method Not Allowed (405)");
    case 406:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:notAcceptable", L"Not Acceptable (406)");
    case 407:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:proxyAuthenticationRequired", L"Proxy Authentication Required (407)");
    case 408:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:requestTimeout", L"Request Timeout (408)");
    case 409:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:conflict", L"Conflict (409)");
    case 410:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:gone", L"Gone (410)");
    case 411:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:lengthRequired", L"Length Required (411)");
    case 412:
        return resolveMessageTemplate(
            L"Nelson:webtools:preconditionFailed", L"Precondition Failed (412)");
    case 413:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:payloadTooLarge", L"Payload Too Large (413)");
    case 414:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:uriTooLong", L"URI Too Long (414)");
    case 415:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:unsupportedMediaType", L"Unsupported Media Type (415)");
    case 416:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:rangeSatisfiable", L"Range Not Satisfiable (416)");
    case 417:
        return resolveMessageTemplate(
            L"Nelson:webtools:expectationFailed", L"Expectation Failed (417)");
    case 418:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:iMTeapot", L"I'm a teapot (418)");
    case 421:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:misdirectedRequest", L"Misdirected Request (421)");
    case 422:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:unprocessableEntity", L"Unprocessable Entity (422)");
    case 423:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:locked", L"Locked (423)");
    case 424:
        return resolveMessageTemplate(
            L"Nelson:webtools:failedDependency", L"Failed Dependency (424)");
    case 425:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:tooEarly", L"Too Early (425)");
    case 426:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:upgradeRequired", L"Upgrade Required (426)");
    case 428:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:preconditionRequired", L"Precondition Required (428)");
    case 429:
        return resolveMessageTemplate(
            L"Nelson:webtools:tooManyRequests", L"Too Many Requests (429)");
    case 431:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:requestHeaderFieldsToo", L"Request Header Fields Too Large (431)");
    case 451:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:unavailableLegalReasons", L"Unavailable For Legal Reasons (451)");
    case 500:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:internalServerError", L"Internal Server Error (500)");
    case 501:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:notImplemented", L"Not Implemented (501)");
    case 502:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:badGateway", L"Bad Gateway (502)");
    case 503:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:serviceUnavailable", L"Service Unavailable (503)");
    case 504:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:gatewayTimeout", L"Gateway Timeout (504)");
    case 505:
        return resolveMessageTemplate(
            L"Nelson:webtools:httpVersionSupported", L"HTTP Version Not Supported (505)");
    case 506:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:variantAlsoNegotiates", L"Variant Also Negotiates (506)");
    case 507:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:insufficientStorage", L"Insufficient Storage (507)");
    case 508:
        return Nelson::resolveMessageTemplate(
            L"Nelson:webtools:loopDetected", L"Loop Detected (508)");
    case 510:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:notExtended", L"Not Extended(510)");
    case 511:
        return Nelson::resolveMessageTemplate(L"Nelson:webtools:networkAuthenticationRequired",
            L"Network Authentication Required (511)");
    default: {
        if (response_code >= 400 && response_code < 500) {
            return formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:webtools:httpClientErrorCode", L"HTTP Client error code: %d"),
                response_code);
        }
        if (response_code >= 500 && response_code < 600) {
            return formatI18nMessage(
                Nelson::resolveMessageTemplate(
                    L"Nelson:webtools:httpServerErrorCode", L"HTTP Server error code: %d"),
                response_code);
        }
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
