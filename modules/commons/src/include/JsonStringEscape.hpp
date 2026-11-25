//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
//=============================================================================
namespace Nelson {
//=============================================================================
// Escape a UTF-8 string for embedding inside a JSON double-quoted string.
// Produces standard JSON: the short escapes for " \ \b \f \n \r \t and \u00XX
// for any other control character below 0x20; all other bytes pass through.
// Shared util replacing the per-module hand-rolled copies.
inline std::string
escapeJsonString(const std::string& s)
{
    static const char* kHex = "0123456789abcdef";
    std::string r;
    r.reserve(s.size() + 8);
    for (char c : s) {
        const unsigned char u = static_cast<unsigned char>(c);
        switch (c) {
        case '"':
            r += "\\\"";
            continue;
        case '\\':
            r += "\\\\";
            continue;
        case '\b':
            r += "\\b";
            continue;
        case '\f':
            r += "\\f";
            continue;
        case '\n':
            r += "\\n";
            continue;
        case '\r':
            r += "\\r";
            continue;
        case '\t':
            r += "\\t";
            continue;
        default:
            break;
        }
        if (u < 0x20) {
            r += "\\u00";
            r += kHex[(u >> 4) & 0x0F];
            r += kHex[u & 0x0F];
            continue;
        }
        r += c;
    }
    return r;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
