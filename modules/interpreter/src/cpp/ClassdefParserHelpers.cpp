//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ClassdefParserHelpers.hpp"
#include <algorithm>
#include <cctype>
#include <sstream>
//=============================================================================
namespace Nelson::classdef_detail {
std::string
trim(const std::string& input)
{
    size_t first = 0;
    while (first < input.size() && std::isspace(static_cast<unsigned char>(input[first])) != 0) {
        ++first;
    }
    size_t last = input.size();
    while (last > first && std::isspace(static_cast<unsigned char>(input[last - 1])) != 0) {
        --last;
    }
    return input.substr(first, last - first);
}
//=============================================================================
std::string
tolowerString(std::string value)
{
    std::transform(value.begin(), value.end(), value.begin(),
        [](unsigned char ch) { return static_cast<char>(std::tolower(ch)); });
    return value;
}
//=============================================================================
bool
equalsIgnoreCase(const std::string& left, const std::string& right)
{
    return tolowerString(left) == tolowerString(right);
}
//=============================================================================
std::string
classNameLeaf(const std::string& className)
{
    size_t pos = className.find_last_of('.');
    if (pos == std::string::npos || pos + 1 >= className.size()) {
        return className;
    }
    return className.substr(pos + 1);
}
//=============================================================================
std::string
externalRootName()
{
    return "nelson";
}
//=============================================================================
std::string
copyableMixinName()
{
    return NLS_MIXIN_COPYABLE;
}
//=============================================================================
std::string
customDisplayMixinName()
{
    return NLS_MIXIN_CUSTOM_DISPLAY;
}
//=============================================================================
std::string
setGetMixinName()
{
    return NLS_MIXIN_SET_GET;
}
//=============================================================================
std::string
setGetExactNamesMixinName()
{
    return NLS_MIXIN_SET_GET_EXACT_NAMES;
}
//=============================================================================
std::string
redefinesParenMixinName()
{
    return NLS_MIXIN_INDEXING_REDEFINES_PAREN;
}
//=============================================================================
std::string
redefinesBraceMixinName()
{
    return NLS_MIXIN_INDEXING_REDEFINES_BRACE;
}
//=============================================================================
std::string
redefinesDotMixinName()
{
    return NLS_MIXIN_INDEXING_REDEFINES_DOT;
}
//=============================================================================
std::string
scalarMixinName()
{
    return NLS_MIXIN_SCALAR;
}
//=============================================================================
bool
equalsClassNameOrLeaf(const std::string& name, const std::string& className)
{
    return equalsIgnoreCase(name, className) || equalsIgnoreCase(name, classNameLeaf(className));
}
//=============================================================================
bool
startsWithKeyword(const std::string& line, const std::string& keyword)
{
    const std::string value = trim(line);
    if (value.size() < keyword.size() || value.compare(0, keyword.size(), keyword) != 0) {
        return false;
    }
    if (value.size() == keyword.size()) {
        return true;
    }
    const unsigned char next = static_cast<unsigned char>(value[keyword.size()]);
    return std::isspace(next) != 0 || next == '(' || next == ';' || next == ',';
}
//=============================================================================
// A single quote opens a character array unless it directly follows a value
// (an identifier char, a closing bracket, a dot or another quote), in which
// case it is the transpose operator. Distinguishing the two is required so a
// trailing comment after a transpose (e.g. "[1 2 3]' % note") is recognized.
bool
singleQuoteOpensCharArray(char previousChar)
{
    const bool followsValue = (previousChar >= 'a' && previousChar <= 'z')
        || (previousChar >= 'A' && previousChar <= 'Z')
        || (previousChar >= '0' && previousChar <= '9') || previousChar == '_'
        || previousChar == ')' || previousChar == ']' || previousChar == '}' || previousChar == '\''
        || previousChar == '.';
    return !followsValue;
}
//=============================================================================
std::string
stripComment(const std::string& line)
{
    bool inSingle = false;
    bool inDouble = false;
    for (size_t k = 0; k < line.size(); ++k) {
        const char ch = line[k];
        if (inSingle) {
            if (ch == '\'') {
                if (k + 1 < line.size() && line[k + 1] == '\'') {
                    ++k;
                } else {
                    inSingle = false;
                }
            }
            continue;
        }
        if (inDouble) {
            if (ch == '"') {
                if (k + 1 < line.size() && line[k + 1] == '"') {
                    ++k;
                } else {
                    inDouble = false;
                }
            }
            continue;
        }
        if (ch == '\'') {
            if (singleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                inSingle = true;
            }
        } else if (ch == '"') {
            inDouble = true;
        } else if (ch == '%') {
            return line.substr(0, k);
        }
    }
    return line;
}
//=============================================================================
std::string
withoutTrailingStatementSeparator(std::string value)
{
    value = trim(value);
    while (!value.empty() && (value.back() == ';' || value.back() == ',')) {
        value.pop_back();
        value = trim(value);
    }
    return value;
}
//=============================================================================
std::vector<std::string>
splitLines(const std::string& source)
{
    std::vector<std::string> lines;
    std::stringstream stream(source);
    std::string line;
    while (std::getline(stream, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        lines.push_back(line);
    }
    if (source.empty() || source.back() == '\n') {
        return lines;
    }
    return lines;
}
//=============================================================================
// Blank out block-comment lines (%{ ... %}) so the structural scanner never
// sees their content as declarations. A block comment opens/closes only on a
// line whose trimmed content is exactly "%{" / "%}"; nesting is honoured.
std::vector<std::string>
stripBlockComments(std::vector<std::string> lines)
{
    int depth = 0;
    for (auto& line : lines) {
        const std::string trimmed = trim(line);
        if (depth == 0) {
            if (trimmed == "%{") {
                ++depth;
                line.clear();
            }
            continue;
        }
        if (trimmed == "%{") {
            ++depth;
        } else if (trimmed == "%}") {
            --depth;
        }
        line.clear();
    }
    return lines;
}
//=============================================================================
// Index of the "..." line-continuation marker outside string literals, or
// npos. Anything after "..." is a comment (so trailing text is allowed);
// a "%" comment encountered first means there is no continuation.
size_t
findLineContinuation(const std::string& line)
{
    bool inSingle = false;
    bool inDouble = false;
    for (size_t k = 0; k < line.size(); ++k) {
        const char ch = line[k];
        if (inSingle) {
            if (ch == '\'') {
                if (k + 1 < line.size() && line[k + 1] == '\'') {
                    ++k;
                } else {
                    inSingle = false;
                }
            }
            continue;
        }
        if (inDouble) {
            if (ch == '"') {
                if (k + 1 < line.size() && line[k + 1] == '"') {
                    ++k;
                } else {
                    inDouble = false;
                }
            }
            continue;
        }
        if (ch == '\'') {
            if (singleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                inSingle = true;
            }
        } else if (ch == '"') {
            inDouble = true;
        } else if (ch == '%') {
            return std::string::npos;
        } else if (ch == '.' && k + 2 < line.size() && line[k + 1] == '.' && line[k + 2] == '.') {
            return k;
        }
    }
    return std::string::npos;
}
//=============================================================================
bool
hasLineContinuation(const std::string& line)
{
    return findLineContinuation(line) != std::string::npos;
}
//=============================================================================
std::string
withoutLineContinuation(const std::string& line)
{
    const size_t dots = findLineContinuation(line);
    if (dots == std::string::npos) {
        return trim(stripComment(line));
    }
    return trim(line.substr(0, dots));
}
//=============================================================================
// Net change of ( [ { nesting over the code portion of a line (string
// literals and %/... comments excluded). Positive means brackets are left
// open at end of line, i.e. the logical line continues implicitly.
int
bracketDepthDelta(const std::string& line)
{
    bool inSingle = false;
    bool inDouble = false;
    int depth = 0;
    for (size_t k = 0; k < line.size(); ++k) {
        const char ch = line[k];
        if (inSingle) {
            if (ch == '\'') {
                if (k + 1 < line.size() && line[k + 1] == '\'') {
                    ++k;
                } else {
                    inSingle = false;
                }
            }
            continue;
        }
        if (inDouble) {
            if (ch == '"') {
                if (k + 1 < line.size() && line[k + 1] == '"') {
                    ++k;
                } else {
                    inDouble = false;
                }
            }
            continue;
        }
        if (ch == '\'') {
            if (singleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                inSingle = true;
            }
        } else if (ch == '"') {
            inDouble = true;
        } else if (ch == '%') {
            break;
        } else if (ch == '.' && k + 2 < line.size() && line[k + 1] == '.' && line[k + 2] == '.') {
            break;
        } else if (ch == '(' || ch == '[' || ch == '{') {
            ++depth;
        } else if (ch == ')' || ch == ']' || ch == '}') {
            --depth;
        }
    }
    return depth;
}
//=============================================================================
// Fold a logical line, honouring both explicit "..." continuations and
// implicit continuations where brackets stay open across a newline (matrix
// rows, multi-line calls). An explicit continuation joins with a space; an
// implicit bracket continuation preserves the newline so that a matrix row
// break keeps its meaning.
std::string
joinedContinuationLine(
    const std::vector<std::string>& lines, size_t first, size_t limit, size_t& last)
{
    std::string result;
    last = first;
    int depth = 0;
    bool previousExplicit = false;
    for (size_t k = first; k < limit && k < lines.size(); ++k) {
        const bool explicitContinuation = hasLineContinuation(lines[k]);
        const std::string piece = explicitContinuation ? withoutLineContinuation(lines[k])
                                                       : trim(stripComment(lines[k]));
        if (!result.empty()) {
            result += (depth > 0 && !previousExplicit) ? '\n' : ' ';
        }
        result += piece;
        last = k;
        depth += bracketDepthDelta(lines[k]);
        previousExplicit = explicitContinuation;
        if (!explicitContinuation && depth <= 0) {
            break;
        }
    }
    return result;
}
//=============================================================================
// Split a single logical line into statements at top-level ";" / "," (i.e.
// outside strings, %-comments and ( [ { nesting; transpose-aware). Used to
// expand a fully inline block such as "properties; X = 1; Y = 2; end".
std::vector<std::string>
splitTopLevelSeparators(const std::string& line)
{
    std::vector<std::string> parts;
    std::string current;
    bool inSingle = false;
    bool inDouble = false;
    int depth = 0;
    for (size_t k = 0; k < line.size(); ++k) {
        const char ch = line[k];
        if (inSingle) {
            current += ch;
            if (ch == '\'') {
                if (k + 1 < line.size() && line[k + 1] == '\'') {
                    current += line[++k];
                } else {
                    inSingle = false;
                }
            }
            continue;
        }
        if (inDouble) {
            current += ch;
            if (ch == '"') {
                if (k + 1 < line.size() && line[k + 1] == '"') {
                    current += line[++k];
                } else {
                    inDouble = false;
                }
            }
            continue;
        }
        if (ch == '%') {
            break;
        }
        if (ch == '\'') {
            if (singleQuoteOpensCharArray(k > 0 ? line[k - 1] : '\0')) {
                inSingle = true;
            }
            current += ch;
            continue;
        }
        if (ch == '"') {
            inDouble = true;
            current += ch;
            continue;
        }
        if (ch == '(' || ch == '[' || ch == '{') {
            ++depth;
            current += ch;
            continue;
        }
        if (ch == ')' || ch == ']' || ch == '}') {
            --depth;
            current += ch;
            continue;
        }
        if ((ch == ';' || ch == ',') && depth == 0) {
            parts.push_back(trim(current));
            current.clear();
            continue;
        }
        current += ch;
    }
    if (!trim(current).empty()) {
        parts.push_back(trim(current));
    }
    return parts;
}
//=============================================================================
// Expand a fully inline property / event / enumeration block written on one
// physical line ("properties; X = 1; end") into the equivalent multi-line
// form the block scanner expects. Only a line that both starts with such a
// block keyword and closes with a top-level "end" is touched; multi-line
// blocks and method bodies pass through unchanged.
std::vector<std::string>
expandInlineBlocks(const std::vector<std::string>& lines)
{
    std::vector<std::string> result;
    for (const auto& line : lines) {
        const std::string trimmed = trim(stripComment(line));
        const bool isInlineBlockCandidate = startsWithKeyword(trimmed, CLASSDEF_KEYWORD_PROPERTIES)
            || startsWithKeyword(trimmed, CLASSDEF_KEYWORD_EVENTS)
            || startsWithKeyword(trimmed, CLASSDEF_KEYWORD_ENUMERATION);
        if (isInlineBlockCandidate) {
            const std::vector<std::string> parts = splitTopLevelSeparators(trimmed);
            if (parts.size() >= 2 && parts.back() == CLASSDEF_KEYWORD_END) {
                for (const auto& part : parts) {
                    result.push_back(part);
                }
                continue;
            }
        }
        result.push_back(line);
    }
    return result;
}
//=============================================================================
std::string
joinLines(const std::vector<std::string>& lines, size_t first, size_t last)
{
    std::string result;
    for (size_t k = first; k <= last && k < lines.size(); ++k) {
        result += lines[k];
        result += '\n';
    }
    return result;
}
//=============================================================================
stringVector
splitCommaList(const std::string& value)
{
    stringVector result;
    std::string current;
    int depth = 0;
    bool inQuote = false;
    for (char ch : value) {
        if (ch == '\'') {
            inQuote = !inQuote;
        } else if (!inQuote && (ch == '(' || ch == '[' || ch == '{')) {
            ++depth;
        } else if (!inQuote && (ch == ')' || ch == ']' || ch == '}') && depth > 0) {
            --depth;
        }
        if (!inQuote && depth == 0 && (ch == ',' || ch == '&')) {
            std::string item = trim(current);
            if (!item.empty()) {
                result.push_back(item);
            }
            current.clear();
        } else {
            current += ch;
        }
    }
    std::string item = trim(current);
    if (!item.empty()) {
        result.push_back(item);
    }
    return result;
}
//=============================================================================
bool
isIdentifierStart(char ch)
{
    return std::isalpha(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
bool
isIdentifierPart(char ch)
{
    return std::isalnum(static_cast<unsigned char>(ch)) != 0 || ch == '_';
}
//=============================================================================
std::string
parseLeadingIdentifier(const std::string& value)
{
    std::string trimmed = trim(value);
    if (trimmed.empty() || !isIdentifierStart(trimmed[0])) {
        return {};
    }
    size_t pos = 1;
    while (pos < trimmed.size() && isIdentifierPart(trimmed[pos])) {
        ++pos;
    }
    return trimmed.substr(0, pos);
}
//=============================================================================
std::string
parseLeadingQualifiedIdentifier(const std::string& value)
{
    std::string trimmed = trim(value);
    std::string result = parseLeadingIdentifier(trimmed);
    if (result.empty()) {
        return {};
    }
    size_t pos = result.size();
    while (pos < trimmed.size() && trimmed[pos] == '.') {
        size_t partStart = pos + 1;
        if (partStart >= trimmed.size() || !isIdentifierStart(trimmed[partStart])) {
            break;
        }
        size_t partEnd = partStart + 1;
        while (partEnd < trimmed.size() && isIdentifierPart(trimmed[partEnd])) {
            ++partEnd;
        }
        result += trimmed.substr(pos, partEnd - pos);
        pos = partEnd;
    }
    return result;
}
//=============================================================================
} // namespace Nelson::classdef_detail
//=============================================================================
