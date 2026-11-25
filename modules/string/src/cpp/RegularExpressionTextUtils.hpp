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
// Pure text utilities extracted from RegularExpression.cpp. Each depends only on
// the standard library and StringHelpers: no PCRE, no file-local regex types, no
// ArrayOf/Evaluator. Header-only (inline) so the single consumer keeps internal
// linkage behavior without a separate translation unit.
//=============================================================================
#include <cwctype>
#include <set>
#include <string>
#include <vector>
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline bool
isAllLower(const std::wstring& s)
{
    bool hasAlpha = false;
    for (wchar_t c : s) {
        if (std::iswalpha(c)) {
            hasAlpha = true;
            if (!std::iswlower(c)) {
                return false;
            }
        }
    }
    return hasAlpha;
}
//=============================================================================
inline bool
isAllUpper(const std::wstring& s)
{
    bool hasAlpha = false;
    for (wchar_t c : s) {
        if (std::iswalpha(c)) {
            hasAlpha = true;
            if (!std::iswupper(c)) {
                return false;
            }
        }
    }
    return hasAlpha;
}
//=============================================================================
inline std::wstring
toCaseLike(const std::wstring& replacement, const std::wstring& original)
{
    std::wstring res = replacement;
    if (isAllUpper(original)) {
        StringHelpers::to_upper(res);
    } else if (isAllLower(original)) {
        StringHelpers::to_lower(res);
    } else if (!original.empty() && std::iswupper(original[0]) && !res.empty()) {
        StringHelpers::to_lower(res);
        res[0] = static_cast<wchar_t>(std::towupper(res[0]));
    }
    return res;
}
//=============================================================================
inline std::wstring
escapeRegex(const std::wstring& input)
{
    const std::wstring special = L"\\.^$|()[]{}*+?";
    std::wstring out;
    for (wchar_t c : input) {
        if (special.find(c) != std::wstring::npos) {
            out.push_back(L'\\');
        }
        out.push_back(c);
    }
    return out;
}
//=============================================================================
inline std::wstring
wildcardToRegex(const std::wstring& input)
{
    std::wstring out;
    for (wchar_t c : input) {
        if (c == L'*') {
            out.append(L".*");
        } else if (c == L'?') {
            out.push_back(L'.');
        } else {
            out.append(escapeRegex(std::wstring(1, c)));
        }
    }
    return out;
}
//=============================================================================
inline std::wstring
lower(const std::wstring& s)
{
    return StringHelpers::to_lower_copy(s);
}
//=============================================================================
inline void
replaceAll(std::wstring& text, const std::wstring& from, const std::wstring& to)
{
    if (from.empty()) {
        return;
    }
    size_t pos = 0;
    while ((pos = text.find(from, pos)) != std::wstring::npos) {
        text.replace(pos, from.size(), to);
        pos += to.size();
    }
}
//=============================================================================
inline std::wstring
quoteNelsonString(const std::wstring& value)
{
    std::wstring q = L"'";
    for (wchar_t c : value) {
        q.push_back(c);
        if (c == L'\'') {
            q.push_back(L'\'');
        }
    }
    q.push_back(L'\'');
    return q;
}
//=============================================================================
inline std::wstring
translatePosixWordAnchors(const std::wstring& pattern)
{
    // PCRE does not know the POSIX word anchors \< and \>. Translate them to lookarounds,
    // leaving escaped backslashes and character classes intact.
    if (pattern.find(L'<') == std::wstring::npos && pattern.find(L'>') == std::wstring::npos) {
        return pattern;
    }
    std::wstring out;
    out.reserve(pattern.size() + 8);
    bool inClass = false;
    for (size_t i = 0; i < pattern.size();) {
        wchar_t c = pattern[i];
        if (c == L'\\' && i + 1 < pattern.size()) {
            wchar_t next = pattern[i + 1];
            if (!inClass && next == L'<') {
                out.append(L"\\b(?=\\w)");
                i += 2;
                continue;
            }
            if (!inClass && next == L'>') {
                out.append(L"(?<=\\w)\\b");
                i += 2;
                continue;
            }
            out.push_back(c);
            out.push_back(next);
            i += 2;
            continue;
        }
        if (!inClass && c == L'[') {
            inClass = true;
        } else if (inClass && c == L']') {
            inClass = false;
        }
        out.push_back(c);
        ++i;
    }
    return out;
}
//=============================================================================
inline std::wstring
makeLenientPattern(const std::wstring& pattern)
{
    // PCRE2 rejects a pattern with an unbalanced metacharacter. This pass escapes '(' and
    // ')' without a partner and a '[' whose class is never terminated, so they match
    // literally. It only runs after a compile failure. If nothing can be rescued the
    // pattern is returned unchanged and the caller reports the original PCRE error.
    const size_t len = pattern.size();
    std::set<size_t> escapePositions;
    std::vector<size_t> openParens;
    bool inClass = false;
    size_t classContentStart = 0;
    size_t classOpenIdx = 0;
    for (size_t i = 0; i < len;) {
        wchar_t c = pattern[i];
        if (c == L'\\') {
            i += 2;
            continue;
        }
        if (inClass) {
            // The first ']' after '[' (or '[^') is a literal member, not the terminator.
            if (c == L']' && i > classContentStart) {
                inClass = false;
            }
            ++i;
            continue;
        }
        if (c == L'[') {
            inClass = true;
            classOpenIdx = i;
            classContentStart = i + 1;
            if (classContentStart < len && pattern[classContentStart] == L'^') {
                ++classContentStart;
            }
            ++i;
            continue;
        }
        if (c == L'(') {
            openParens.push_back(i);
        } else if (c == L')') {
            if (!openParens.empty()) {
                openParens.pop_back();
            } else {
                escapePositions.insert(i);
            }
        }
        ++i;
    }
    for (size_t idx : openParens) {
        escapePositions.insert(idx);
    }
    if (inClass) {
        escapePositions.insert(classOpenIdx);
    }
    if (escapePositions.empty()) {
        return pattern;
    }
    std::wstring out;
    out.reserve(len + escapePositions.size());
    for (size_t i = 0; i < len; ++i) {
        if (escapePositions.find(i) != escapePositions.end()) {
            out.push_back(L'\\');
        }
        out.push_back(pattern[i]);
    }
    return out;
}
//=============================================================================
inline bool
isAscii(const std::wstring& text)
{
    for (wchar_t c : text) {
        if (c > 127) {
            return false;
        }
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
