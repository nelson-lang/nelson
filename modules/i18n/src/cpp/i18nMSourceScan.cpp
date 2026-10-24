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
#include <regex>
#include "i18nSourceScanners.hpp"
//=============================================================================
// Language-aware .m source scanners. A character scanner (not a regex) extracts
// _('...') / _("...") messages. It understands .m string literals ('' and ""
// doubling), comments, block comments, the transpose operator and line
// continuations, so only real messages are captured. Escapes are decoded with the
// C/Python rules so catalog keys do not change.
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isAsciiLetter(char c);
static bool
isAsciiDigit(char c);
static bool
isTransposeContext(char prev);
static bool
parseQuoted(
    const std::string& line, size_t startpos, char quote, std::string& text, size_t& newpos);
static std::string
decodeCEscapes(const std::string& s);
static std::string
trimmedLine(const std::string& line);
static void
scanMLine(const std::string& line, bool& expectArg, std::set<std::string>& msgs);
static void
addIdTemplate(std::map<std::string, std::vector<std::string>>& idTemplates, const std::string& id,
    const std::string& tmpl);
//=============================================================================
void
scanMFileForUiStrings(const std::string& text, std::set<std::string>& texts)
{
    if (text.find("_(") == std::string::npos) {
        return;
    }
    std::string content;
    content.reserve(text.size());
    for (char c : text) {
        if (c != '\r') {
            content.push_back(c);
        }
    }
    bool inBlockComment = false;
    bool expectArg = false; // true right after a '_(' : the next literal is a msgid
    size_t start = 0;
    size_t n = content.size();
    while (start <= n) {
        size_t eol = content.find('\n', start);
        std::string line
            = content.substr(start, (eol == std::string::npos) ? std::string::npos : eol - start);
        std::string trimmed = trimmedLine(line);
        if (inBlockComment) {
            if (trimmed == "%}") {
                inBlockComment = false;
            }
        } else if (trimmed == "%{") {
            inBlockComment = true;
        } else {
            scanMLine(line, expectArg, texts);
        }
        if (eol == std::string::npos) {
            break;
        }
        start = eol + 1;
    }
}
//=============================================================================
void
scanMFileForIdTemplates(
    const std::string& text, std::map<std::string, std::vector<std::string>>& idTemplates)
{
    if (text.find("error(") == std::string::npos && text.find("warning(") == std::string::npos) {
        return;
    }
    std::string content;
    content.reserve(text.size());
    for (char c : text) {
        if (c != '\r') {
            content.push_back(c);
        }
    }
    // Collapse line continuations so an error id and its literal template split
    // across lines match as one call:
    //   error('mod:id', ...
    //     'A literal template.')
    static const std::regex continuationPattern("\\.\\.\\.[ \\t]*\\n[ \\t]*");
    content = std::regex_replace(content, continuationPattern, std::string());
    // Only a literal identifier plus a literal single-quoted template is captured (a
    // concatenation or a sprintf call is not a plain template). The identifier is any
    // valid MException id ("<namespace>:<mnemonic>..."); the namespace filter keeps a
    // module's own keys.
    static const std::regex errorPattern(
        "(?:error|warning)\\s*\\(\\s*'([A-Za-z_][A-Za-z0-9_]*(?::[A-Za-z0-9_]+)+)'\\s*,\\s*'((?:["
        "^']|'')*)'");
    // nmm_error('reason', 'tmpl', ...) raises error(['Nelson:nmm:', reason], tmpl,
    // ...) so its bare literal template is keyed by Nelson:nmm:<reason>.
    static const std::regex nmmPattern("nmm_error\\s*\\(\\s*'(\\w+)'\\s*,\\s*'((?:[^']|'')*)'");
    auto unescapeTemplate = [](const std::string& raw) {
        std::string out;
        out.reserve(raw.size());
        for (size_t i = 0; i < raw.size(); ++i) {
            if (raw[i] == '\'' && i + 1 < raw.size() && raw[i + 1] == '\'') {
                out.push_back('\'');
                ++i;
            } else {
                out.push_back(raw[i]);
            }
        }
        return decodeCEscapes(out);
    };
    for (auto it = std::sregex_iterator(content.begin(), content.end(), errorPattern);
        it != std::sregex_iterator(); ++it) {
        addIdTemplate(idTemplates, (*it)[1].str(), unescapeTemplate((*it)[2].str()));
    }
    for (auto it = std::sregex_iterator(content.begin(), content.end(), nmmPattern);
        it != std::sregex_iterator(); ++it) {
        addIdTemplate(
            idTemplates, "Nelson:nmm:" + (*it)[1].str(), unescapeTemplate((*it)[2].str()));
    }
}
//=============================================================================
static void
addIdTemplate(std::map<std::string, std::vector<std::string>>& idTemplates, const std::string& id,
    const std::string& tmpl)
{
    std::vector<std::string>& templates = idTemplates[id];
    if (std::find(templates.begin(), templates.end(), tmpl) == templates.end()) {
        templates.push_back(tmpl);
    }
}
//=============================================================================
// Character scanner for a single physical line. expectArg is carried in/out so
// that a message split across a line continuation ( _(\n 'text') ) is caught.
static void
scanMLine(const std::string& line, bool& expectArg, std::set<std::string>& msgs)
{
    size_t n = line.size();
    size_t pos = 0;
    char prev = '\0'; // previous significant char (for transpose disambiguation)
    while (pos < n) {
        char c = line[pos];
        if (c == ' ' || c == '\t') {
            ++pos; // whitespace: keep prev and expectArg
            continue;
        }
        if (c == '%') {
            return;
        }
        if (c == '.' && pos + 2 < n && line[pos + 1] == '.' && line[pos + 2] == '.') {
            return;
        }
        if (c == '\'') {
            if (expectArg || !isTransposeContext(prev)) {
                std::string text;
                size_t newpos = 0;
                if (!parseQuoted(line, pos, '\'', text, newpos)) {
                    expectArg = false; // unterminated on line: abandon safely
                    return;
                }
                if (expectArg) {
                    std::string decoded = decodeCEscapes(text);
                    if (!decoded.empty()) {
                        msgs.insert(decoded);
                    }
                    expectArg = false;
                }
                pos = newpos;
                prev = ')'; // a completed string behaves like a value
                continue;
            }
            ++pos;
            prev = '\'';
            continue;
        }
        if (c == '"') {
            std::string text;
            size_t newpos = 0;
            if (!parseQuoted(line, pos, '"', text, newpos)) {
                expectArg = false;
                return;
            }
            if (expectArg) {
                std::string decoded = decodeCEscapes(text);
                if (!decoded.empty()) {
                    msgs.insert(decoded);
                }
                expectArg = false;
            }
            pos = newpos;
            prev = ')';
            continue;
        }
        if (isAsciiLetter(c) || c == '_') {
            size_t j = pos + 1;
            while (j < n && (isAsciiLetter(line[j]) || line[j] == '_' || isAsciiDigit(line[j]))) {
                ++j;
            }
            std::string ident = line.substr(pos, j - pos);
            prev = line[j - 1];
            pos = j;
            if (ident == "_") {
                size_t p2 = pos;
                while (p2 < n && (line[p2] == ' ' || line[p2] == '\t')) {
                    ++p2;
                }
                if (p2 < n && line[p2] == '(') {
                    expectArg = true;
                    pos = p2 + 1;
                    prev = '(';
                    continue;
                }
            }
            expectArg = false;
            continue;
        }
        if (isAsciiDigit(c)) {
            prev = c;
            ++pos;
            expectArg = false;
            continue;
        }
        prev = c;
        ++pos;
        expectArg = false;
    }
}
//=============================================================================
static bool
isAsciiLetter(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}
//=============================================================================
static bool
isAsciiDigit(char c)
{
    return c >= '0' && c <= '9';
}
//=============================================================================
// A quote is the transpose operator (not a string start) when it directly
// follows a value: an identifier char, a closing bracket, a dot, or a quote.
static bool
isTransposeContext(char prev)
{
    if (prev == '\0') {
        return false;
    }
    if (isAsciiLetter(prev) || isAsciiDigit(prev)) {
        return true;
    }
    return prev == ')' || prev == ']' || prev == '}' || prev == '.' || prev == '\'' || prev == '"';
}
//=============================================================================
// Parses a .m string from the opening quote. A doubled quote is a literal quote.
// Strings never span physical lines.
static bool
parseQuoted(const std::string& line, size_t startpos, char quote, std::string& text, size_t& newpos)
{
    size_t n = line.size();
    size_t k = startpos + 1;
    std::string buf;
    bool ok = false;
    newpos = startpos + 1;
    while (k < n) {
        char ch = line[k];
        if (ch == quote) {
            if (k + 1 < n && line[k + 1] == quote) {
                buf.push_back(quote);
                k += 2;
            } else {
                newpos = k + 1;
                ok = true;
                break;
            }
        } else {
            buf.push_back(ch);
            ++k;
        }
    }
    text = buf;
    return ok;
}
//=============================================================================
// Decodes C/Python style backslash escapes. Unknown escapes keep the backslash
// (Python rule).
static std::string
decodeCEscapes(const std::string& s)
{
    if (s.find('\\') == std::string::npos) {
        return s;
    }
    std::string out;
    out.reserve(s.size());
    size_t n = s.size();
    size_t i = 0;
    while (i < n) {
        char c = s[i];
        if (c == '\\' && i + 1 < n) {
            char nx = s[i + 1];
            switch (nx) {
            case 'n':
                out.push_back('\n');
                break;
            case 't':
                out.push_back('\t');
                break;
            case 'r':
                out.push_back('\r');
                break;
            case '\\':
                out.push_back('\\');
                break;
            case '"':
                out.push_back('"');
                break;
            case '\'':
                out.push_back('\'');
                break;
            case 'a':
                out.push_back('\a');
                break;
            case 'b':
                out.push_back('\b');
                break;
            case 'f':
                out.push_back('\f');
                break;
            case 'v':
                out.push_back('\v');
                break;
            default:
                out.push_back('\\');
                out.push_back(nx);
                break;
            }
            i += 2;
        } else {
            out.push_back(c);
            ++i;
        }
    }
    return out;
}
//=============================================================================
static std::string
trimmedLine(const std::string& line)
{
    size_t b = 0;
    size_t e = line.size();
    while (b < e && (line[b] == ' ' || line[b] == '\t')) {
        ++b;
    }
    while (e > b && (line[e - 1] == ' ' || line[e - 1] == '\t')) {
        --e;
    }
    return line.substr(b, e - b);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
