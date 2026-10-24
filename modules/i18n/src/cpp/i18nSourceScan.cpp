//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cctype>
#include <cstring>
#include "i18nSourceScanners.hpp"
//=============================================================================
// C/C++ source scanners. All scanners share the same tokenizer skeleton:
// comments and string literals are skipped, keyword calls are located as
// identifiers immediately followed by an argument list, and their arguments
// are split at top-level commas before literal content is decoded.
//=============================================================================
namespace Nelson {
//=============================================================================
// True when a literal looks like an MException identifier ("<ns>:<mnemonic>...":
// at least two ':'-separated components, each a valid identifier, no whitespace).
// Keeps ids of any namespace, not only "Nelson:", out of the text-keyed UI catalog.
static bool
looksLikeMExceptionId(const std::string& s)
{
    if (s.empty() || s.find(':') == std::string::npos) {
        return false;
    }
    size_t start = 0;
    int components = 0;
    while (start <= s.size()) {
        size_t colon = s.find(':', start);
        size_t end = (colon == std::string::npos) ? s.size() : colon;
        if (end == start) {
            return false;
        }
        for (size_t i = start; i < end; ++i) {
            unsigned char ch = (unsigned char)s[i];
            bool ok = (std::isalnum(ch) != 0) || ch == '_';
            if (i == start) {
                ok = (std::isalpha(ch) != 0) || ch == '_';
            }
            if (!ok) {
                return false;
            }
        }
        ++components;
        if (colon == std::string::npos) {
            break;
        }
        start = colon + 1;
    }
    return components >= 2;
}
//=============================================================================
static std::vector<std::string>
splitTopLevelArgs(const std::string& s);
static bool
startsWithStringLiteral(const std::string& arg);
static std::string
identifierLiteral(const std::string& arg);
static std::string
unescapeLiteral(const std::string& raw);
static bool
literalArgContent(const std::string& arg, std::string& out);
static bool
messageArgLiteral(const std::string& arg, bool mFile, std::string& out);
static size_t
skipToCallEnd(const std::string& text, size_t openPos, bool mFile);
//=============================================================================
void
scanSourceForRuntimeIds(const std::string& text, std::set<std::string>& ids)
{
    static const char* kNames[] = { "Error", "ErrorAsCaller", "Warning" };
    size_t n = text.size();
    size_t i = 0;
    while (i < n) {
        char c = text[i];
        if (c == '/' && i + 1 < n && text[i + 1] == '/') {
            size_t j = text.find('\n', i);
            i = (j == std::string::npos) ? n : j;
            continue;
        }
        if (c == '/' && i + 1 < n && text[i + 1] == '*') {
            size_t j = text.find("*/", i + 2);
            i = (j == std::string::npos) ? n : j + 2;
            continue;
        }
        if (c == '"' || c == '\'') {
            char q = c;
            size_t j = i + 1;
            while (j < n) {
                if (text[j] == '\\') {
                    j += 2;
                    continue;
                }
                if (text[j] == q) {
                    ++j;
                    break;
                }
                ++j;
            }
            i = j;
            continue;
        }
        bool matched = false;
        for (const char* nm : kNames) {
            size_t len = std::strlen(nm);
            if (i + len < n && text.compare(i, len, nm) == 0 && text[i + len] == '('
                && (i == 0
                    || !(std::isalnum((unsigned char)text[i - 1]) != 0 || text[i - 1] == '_'))) {
                size_t k = i + len;
                size_t p = skipToCallEnd(text, k, false);
                std::string inner
                    = (p > k + 1) ? text.substr(k + 1, p - 1 - (k + 1)) : std::string();
                std::vector<std::string> args = splitTopLevelArgs(inner);
                if (args.size() >= 2 && !startsWithStringLiteral(args[1])) {
                    std::string id = identifierLiteral(args[0]);
                    if (looksLikeMExceptionId(id)) {
                        ids.insert(id);
                    }
                }
                i = p;
                matched = true;
                break;
            }
        }
        if (!matched) {
            ++i;
        }
    }
}
//=============================================================================
void
scanSourceForTextKeyed(const std::string& text, std::set<std::string>& texts)
{
    static const char* kNames[] = { "_", "_W", "TR" };
    size_t n = text.size();
    size_t i = 0;
    while (i < n) {
        char c = text[i];
        if (c == '/' && i + 1 < n && text[i + 1] == '/') {
            size_t j = text.find('\n', i);
            i = (j == std::string::npos) ? n : j;
            continue;
        }
        if (c == '/' && i + 1 < n && text[i + 1] == '*') {
            size_t j = text.find("*/", i + 2);
            i = (j == std::string::npos) ? n : j + 2;
            continue;
        }
        if (c == '"' || c == '\'') {
            char q = c;
            size_t j = i + 1;
            while (j < n) {
                if (text[j] == '\\') {
                    j += 2;
                    continue;
                }
                if (text[j] == q) {
                    ++j;
                    break;
                }
                ++j;
            }
            i = j;
            continue;
        }
        bool matched = false;
        for (const char* nm : kNames) {
            size_t len = std::strlen(nm);
            if (i + len < n && text.compare(i, len, nm) == 0 && text[i + len] == '('
                && (i == 0
                    || !(std::isalnum((unsigned char)text[i - 1]) != 0 || text[i - 1] == '_'))) {
                size_t k = i + len;
                size_t p = skipToCallEnd(text, k, false);
                std::string inner
                    = (p > k + 1) ? text.substr(k + 1, p - 1 - (k + 1)) : std::string();
                std::vector<std::string> args = splitTopLevelArgs(inner);
                std::string content;
                if (!args.empty() && literalArgContent(args[0], content) && !content.empty()) {
                    texts.insert(content);
                }
                i = p;
                matched = true;
                break;
            }
        }
        if (!matched) {
            ++i;
        }
    }
}
//=============================================================================
void
scanSourceForIdTemplates(
    const std::string& text, std::map<std::string, std::vector<std::string>>& idTemplates)
{
    // Only the identifier-keyed, localizing forms are scanned: Error/ErrorAsCaller/
    // Warning(id, template, ...) and resolveMessageTemplate(id, template). The
    // low-level RaiseError/RaiseWarning(msg, id) is deliberately NOT scanned: it
    // raises a run-time message without resolving the catalog by id, so a literal
    // passed to it is not a localizable template.
    static const char* kNames[] = { "Error", "ErrorAsCaller", "Warning", "resolveMessageTemplate" };
    size_t n = text.size();
    size_t i = 0;
    while (i < n) {
        char c = text[i];
        if (c == '/' && i + 1 < n && text[i + 1] == '/') {
            size_t j = text.find('\n', i);
            i = (j == std::string::npos) ? n : j;
            continue;
        }
        if (c == '/' && i + 1 < n && text[i + 1] == '*') {
            size_t j = text.find("*/", i + 2);
            i = (j == std::string::npos) ? n : j + 2;
            continue;
        }
        if (c == '"' || c == '\'') {
            char q = c;
            size_t j = i + 1;
            while (j < n) {
                if (text[j] == '\\') {
                    j += 2;
                    continue;
                }
                if (text[j] == q) {
                    ++j;
                    break;
                }
                ++j;
            }
            i = j;
            continue;
        }
        bool matched = false;
        for (const char* nm : kNames) {
            size_t len = std::strlen(nm);
            if (i + len < n && text.compare(i, len, nm) == 0
                && (i == 0
                    || !(std::isalnum((unsigned char)text[i - 1]) != 0 || text[i - 1] == '_'))) {
                size_t k = i + len;
                while (k < n && std::isspace((unsigned char)text[k]) != 0) {
                    ++k;
                }
                if (k >= n || text[k] != '(') {
                    continue;
                }
                size_t p = skipToCallEnd(text, k, false);
                std::string inner
                    = (p > k + 1) ? text.substr(k + 1, p - 1 - (k + 1)) : std::string();
                std::vector<std::string> args = splitTopLevelArgs(inner);
                std::string id;
                std::string tmpl;
                if (args.size() >= 2 && literalArgContent(args[0], id) && !id.empty()
                    && literalArgContent(args[1], tmpl) && !tmpl.empty()) {
                    std::vector<std::string>& templates = idTemplates[id];
                    if (std::find(templates.begin(), templates.end(), tmpl) == templates.end()) {
                        templates.push_back(tmpl);
                    }
                }
                // Resume right after the opening parenthesis (not after the whole call):
                // a keyword call nested in another call's arguments (e.g. resolveMessageTemplate
                // inside Error(formatI18nMessage(...))) must be scanned too.
                i = k + 1;
                matched = true;
                break;
            }
        }
        if (!matched) {
            ++i;
        }
    }
}
//=============================================================================
void
scanSourceForErrorMessages(const std::string& text, std::set<std::string>& msgs, bool mFile)
{
    static const char* kCpp[] = { "Error", "Warning", "RaiseError", "RaiseWarning" };
    static const char* kM[] = { "error", "warning" };
    const char** names = mFile ? kM : kCpp;
    size_t count = mFile ? 2u : 4u;
    size_t n = text.size();
    size_t i = 0;
    while (i < n) {
        char c = text[i];
        if (!mFile && c == '/' && i + 1 < n && text[i + 1] == '/') {
            size_t j = text.find('\n', i);
            i = (j == std::string::npos) ? n : j;
            continue;
        }
        if (!mFile && c == '/' && i + 1 < n && text[i + 1] == '*') {
            size_t j = text.find("*/", i + 2);
            i = (j == std::string::npos) ? n : j + 2;
            continue;
        }
        if (mFile && c == '%') {
            size_t j = text.find('\n', i);
            i = (j == std::string::npos) ? n : j;
            continue;
        }
        if (c == '"' || c == '\'') {
            char q = c;
            size_t j = i + 1;
            while (j < n) {
                if (!mFile && text[j] == '\\') {
                    j += 2;
                    continue;
                }
                if (mFile && text[j] == '\'' && q == '\'' && j + 1 < n && text[j + 1] == '\'') {
                    j += 2;
                    continue;
                }
                if (text[j] == q) {
                    ++j;
                    break;
                }
                ++j;
            }
            i = j;
            continue;
        }
        bool matched = false;
        for (size_t ni = 0; ni < count; ++ni) {
            const char* nm = names[ni];
            size_t len = std::strlen(nm);
            if (i + len < n && text.compare(i, len, nm) == 0
                && (i == 0
                    || !(std::isalnum((unsigned char)text[i - 1]) != 0 || text[i - 1] == '_'
                        || text[i - 1] == '.'))) {
                size_t k = i + len;
                while (k < n && std::isspace((unsigned char)text[k]) != 0) {
                    ++k;
                }
                if (k >= n || text[k] != '(') {
                    continue;
                }
                size_t p = skipToCallEnd(text, k, mFile);
                std::string inner
                    = (p > k + 1) ? text.substr(k + 1, p - 1 - (k + 1)) : std::string();
                std::vector<std::string> args = splitTopLevelArgs(inner);
                for (const std::string& a : args) {
                    std::string lit;
                    if (messageArgLiteral(a, mFile, lit) && !lit.empty()) {
                        if (looksLikeMExceptionId(lit)) {
                            continue;
                        }
                        msgs.insert(lit);
                        break;
                    }
                }
                i = p;
                matched = true;
                break;
            }
        }
        if (!matched) {
            ++i;
        }
    }
}
//=============================================================================
// Advances from the opening parenthesis at openPos to just after the matching
// closing one, honouring string literals (C/C++ backslash escapes or .m '' doubling).
static size_t
skipToCallEnd(const std::string& text, size_t openPos, bool mFile)
{
    size_t n = text.size();
    int depth = 0;
    size_t p = openPos;
    char instr = 0;
    while (p < n) {
        char ch = text[p];
        if (instr != 0) {
            if (!mFile && ch == '\\') {
                p += 2;
                continue;
            }
            if (mFile && ch == '\'' && instr == '\'' && p + 1 < n && text[p + 1] == '\'') {
                p += 2;
                continue;
            }
            if (ch == instr) {
                instr = 0;
            }
            ++p;
            continue;
        }
        if (ch == '"' || ch == '\'') {
            instr = ch;
            ++p;
            continue;
        }
        if (ch == '(') {
            ++depth;
        } else if (ch == ')') {
            --depth;
            if (depth == 0) {
                ++p;
                break;
            }
        }
        ++p;
    }
    return p;
}
//=============================================================================
static std::vector<std::string>
splitTopLevelArgs(const std::string& s)
{
    std::vector<std::string> args;
    int depth = 0;
    char instr = 0;
    std::string cur;
    for (size_t i = 0; i < s.size(); ++i) {
        char c = s[i];
        if (instr != 0) {
            cur.push_back(c);
            if (c == '\\' && i + 1 < s.size()) {
                cur.push_back(s[++i]);
                continue;
            }
            if (c == instr) {
                instr = 0;
            }
            continue;
        }
        if (c == '"' || c == '\'') {
            instr = c;
            cur.push_back(c);
            continue;
        }
        if (c == '(' || c == '[' || c == '{') {
            ++depth;
        } else if (c == ')' || c == ']' || c == '}') {
            --depth;
        } else if (c == ',' && depth == 0) {
            args.push_back(cur);
            cur.clear();
            continue;
        }
        cur.push_back(c);
    }
    args.push_back(cur);
    return args;
}
//=============================================================================
static bool
startsWithStringLiteral(const std::string& arg)
{
    size_t i = 0;
    while (i < arg.size() && (std::isspace((unsigned char)arg[i]) != 0)) {
        ++i;
    }
    if (i < arg.size() && arg[i] == 'L') {
        ++i;
    }
    return i < arg.size() && arg[i] == '"';
}
//=============================================================================
static std::string
identifierLiteral(const std::string& arg)
{
    if (!startsWithStringLiteral(arg)) {
        return "";
    }
    size_t q = arg.find('"');
    if (q == std::string::npos) {
        return "";
    }
    size_t e = arg.find('"', q + 1);
    if (e == std::string::npos) {
        return "";
    }
    return arg.substr(q + 1, e - q - 1);
}
//=============================================================================
static std::string
unescapeLiteral(const std::string& raw)
{
    std::string out;
    for (size_t i = 0; i < raw.size(); ++i) {
        if (raw[i] == '\\' && i + 1 < raw.size()) {
            char n = raw[++i];
            switch (n) {
            case 'n':
                out.push_back('\n');
                break;
            case 't':
                out.push_back('\t');
                break;
            case 'r':
                out.push_back('\r');
                break;
            case '"':
                out.push_back('"');
                break;
            case '\\':
                out.push_back('\\');
                break;
            default:
                out.push_back(n);
                break;
            }
        } else {
            out.push_back(raw[i]);
        }
    }
    return out;
}
//=============================================================================
// If arg is a (possibly adjacent-concatenated, optionally L-prefixed) string literal, set out
// to its unescaped content and return true; otherwise return false.
static bool
literalArgContent(const std::string& arg, std::string& out)
{
    std::string content;
    size_t i = 0;
    size_t n = arg.size();
    bool sawLiteral = false;
    while (i < n) {
        while (i < n && (std::isspace((unsigned char)arg[i]) != 0)) {
            ++i;
        }
        if (i >= n) {
            break;
        }
        if (arg[i] == 'L' && i + 1 < n && arg[i + 1] == '"') {
            ++i;
        }
        if (i >= n || arg[i] != '"') {
            return false;
        }
        ++i;
        while (i < n && arg[i] != '"') {
            if (arg[i] == '\\' && i + 1 < n) {
                content.push_back(arg[i]);
                content.push_back(arg[i + 1]);
                i += 2;
                continue;
            }
            content.push_back(arg[i]);
            ++i;
        }
        if (i >= n) {
            return false;
        }
        ++i;
        sawLiteral = true;
    }
    if (!sawLiteral) {
        return false;
    }
    out = unescapeLiteral(content);
    return true;
}
//=============================================================================
// Extract the leading string literal of a call argument, transparently
// unwrapping _(...) / _W(...) / TR(...) / sprintf(...) wrappers. .m literals are
// single-quoted (with '' escapes); C++ literals are (L)"..." possibly
// concatenated. Returns false when the argument does not begin (after wrappers)
// with a string literal (a bare variable, a leading-variable concatenation...).
static bool
messageArgLiteral(const std::string& arg, bool mFile, std::string& out)
{
    size_t i = 0;
    const size_t n = arg.size();
    while (i < n) {
        while (i < n && std::isspace((unsigned char)arg[i]) != 0) {
            ++i;
        }
        if (i >= n) {
            return false;
        }
        char c = arg[i];
        if (c == '_' || std::isalpha((unsigned char)c) != 0) {
            size_t j = i;
            while (j < n && (std::isalnum((unsigned char)arg[j]) != 0 || arg[j] == '_')) {
                ++j;
            }
            std::string name = arg.substr(i, j - i);
            size_t k = j;
            while (k < n && std::isspace((unsigned char)arg[k]) != 0) {
                ++k;
            }
            if (k < n && arg[k] == '('
                && (name == "_" || name == "_W" || name == "TR" || name == "sprintf")) {
                i = k + 1;
                continue;
            }
            return false;
        }
        char q = mFile ? '\'' : '"';
        if (!mFile && c == 'L' && i + 1 < n && arg[i + 1] == '"') {
            ++i;
            c = arg[i];
        }
        if (c != q) {
            return false;
        }
        std::string content;
        ++i;
        while (i < n) {
            char d = arg[i];
            if (mFile) {
                if (d == '\'') {
                    if (i + 1 < n && arg[i + 1] == '\'') {
                        content.push_back('\'');
                        i += 2;
                        continue;
                    }
                    ++i;
                    break;
                }
                content.push_back(d);
                ++i;
            } else {
                if (d == '\\' && i + 1 < n) {
                    content.push_back(d);
                    content.push_back(arg[i + 1]);
                    i += 2;
                    continue;
                }
                if (d == '"') {
                    ++i;
                    break;
                }
                content.push_back(d);
                ++i;
            }
        }
        if (!mFile) {
            while (true) {
                size_t p = i;
                while (p < n && std::isspace((unsigned char)arg[p]) != 0) {
                    ++p;
                }
                if (p < n && arg[p] == 'L') {
                    ++p;
                }
                if (p < n && arg[p] == '"') {
                    i = p + 1;
                    while (i < n) {
                        if (arg[i] == '\\' && i + 1 < n) {
                            content.push_back(arg[i]);
                            content.push_back(arg[i + 1]);
                            i += 2;
                            continue;
                        }
                        if (arg[i] == '"') {
                            ++i;
                            break;
                        }
                        content.push_back(arg[i]);
                        ++i;
                    }
                } else {
                    break;
                }
            }
            out = unescapeLiteral(content);
        } else {
            out = content;
        }
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
