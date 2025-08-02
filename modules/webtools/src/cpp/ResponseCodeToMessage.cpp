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
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
responseCodeToMessage(long response_code)
{
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
    default: {
        if (response_code >= 400 && response_code < 500) {
            return _W("HTTP Client error code: ") + std::to_wstring(response_code);
        }
        if (response_code >= 500 && response_code < 600) {
            return _W("HTTP Server error code: ") + std::to_wstring(response_code);
        }
    } break;
    }
    return {};
}
//=============================================================================
} // namespace Nelson
//=============================================================================
