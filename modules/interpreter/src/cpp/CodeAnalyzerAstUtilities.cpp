//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CodeAnalyzerAstUtilities.hpp"
#include <algorithm>
#include <cctype>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace CodeAnalyzerAst {
    //=============================================================================
    std::string
    trim(const std::string& value)
    {
        size_t first = 0;
        while (first < value.size() && std::isspace(static_cast<unsigned char>(value[first]))) {
            ++first;
        }
        size_t last = value.size();
        while (last > first && std::isspace(static_cast<unsigned char>(value[last - 1]))) {
            --last;
        }
        return value.substr(first, last - first);
    }
    //=============================================================================
    std::string
    toLowerAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    //=============================================================================
    bool
    startsWithWord(const std::string& value, const std::string& word)
    {
        if (value.size() < word.size() || value.compare(0, word.size(), word) != 0) {
            return false;
        }
        return value.size() == word.size()
            || !std::isalnum(static_cast<unsigned char>(value[word.size()]));
    }
    //=============================================================================
    bool
    canStartQuotedString(const std::string& line, size_t quote)
    {
        size_t cursor = quote;
        while (cursor > 0 && std::isspace(static_cast<unsigned char>(line[cursor - 1]))) {
            --cursor;
        }
        if (cursor == 0) {
            return true;
        }
        const char previous = line[cursor - 1];
        return previous == '(' || previous == '[' || previous == '{' || previous == '='
            || previous == ',' || previous == ';' || previous == ':' || previous == '+'
            || previous == '-' || previous == '*' || previous == '/' || previous == '\\'
            || previous == '^' || previous == '<' || previous == '>' || previous == '~'
            || previous == '&' || previous == '|';
    }
    //=============================================================================
    size_t
    codeAnalyzerCommentStart(const std::string& line)
    {
        char stringDelimiter = '\0';
        for (size_t k = 0; k < line.size(); ++k) {
            const char current = line[k];
            if (stringDelimiter != '\0') {
                if (current == stringDelimiter) {
                    if (k + 1 < line.size() && line[k + 1] == stringDelimiter) {
                        ++k;
                        continue;
                    }
                    stringDelimiter = '\0';
                }
                continue;
            }
            if (current == '"' || (current == '\'' && canStartQuotedString(line, k))) {
                stringDelimiter = current;
                continue;
            }
            if (current == '%') {
                return k;
            }
        }
        return std::string::npos;
    }
    //=============================================================================
    std::string
    stripComment(const std::string& line)
    {
        size_t comment = codeAnalyzerCommentStart(line);
        return comment == std::string::npos ? line : line.substr(0, comment);
    }
    //=============================================================================
    std::string
    codeWithoutStringsAndComments(const std::string& line)
    {
        std::string code = stripComment(line);
        char stringDelimiter = '\0';
        for (size_t k = 0; k < code.size(); ++k) {
            const char current = code[k];
            if (stringDelimiter != '\0') {
                code[k] = ' ';
                if (current == stringDelimiter) {
                    if (k + 1 < code.size() && code[k + 1] == stringDelimiter) {
                        code[++k] = ' ';
                        continue;
                    }
                    stringDelimiter = '\0';
                }
                continue;
            }
            if (current == '"' || (current == '\'' && canStartQuotedString(code, k))) {
                stringDelimiter = current;
                code[k] = ' ';
            }
        }
        return code;
    }
    //=============================================================================
    stringVector
    splitNames(const std::string& value)
    {
        stringVector names;
        std::string current;
        for (char c : value) {
            if (c == ',' || std::isspace(static_cast<unsigned char>(c))) {
                std::string name = trim(current);
                if (!name.empty() && name != "~") {
                    names.push_back(name);
                }
                current.clear();
            } else {
                current.push_back(c);
            }
        }
        std::string name = trim(current);
        if (!name.empty() && name != "~") {
            names.push_back(name);
        }
        return names;
    }
    //=============================================================================
    bool
    isIdentifierStart(char c)
    {
        return std::isalpha(static_cast<unsigned char>(c)) || c == '_';
    }
    //=============================================================================
    bool
    isIdentifierChar(char c)
    {
        return std::isalnum(static_cast<unsigned char>(c)) || c == '_';
    }
    //=============================================================================
    size_t
    skipSpaces(const std::string& value, size_t cursor)
    {
        while (cursor < value.size() && std::isspace(static_cast<unsigned char>(value[cursor]))) {
            ++cursor;
        }
        return cursor;
    }
    //=============================================================================
    size_t
    codeAnalyzerAssignmentOperator(const std::string& line)
    {
        size_t cursor = line.find('=');
        while (cursor != std::string::npos) {
            const bool previousIsComparator = cursor > 0
                && (line[cursor - 1] == '<' || line[cursor - 1] == '>' || line[cursor - 1] == '~'
                    || line[cursor - 1] == '=');
            const bool nextIsComparator = cursor + 1 < line.size() && line[cursor + 1] == '=';
            if (!previousIsComparator && !nextIsComparator) {
                return cursor;
            }
            cursor = line.find('=', cursor + 1);
        }
        return std::string::npos;
    }
    //=============================================================================
    const std::set<std::string>&
    reservedWords()
    {
        static const std::set<std::string> words
            = { "break", "case", "catch", "classdef", "continue", "else", "elseif", "end", "for",
                  "function", "if", "otherwise", "return", "switch", "try", "while", "true",
                  "false", "global", "persistent", "arguments", "import" };
        return words;
    }
    //=============================================================================
    int
    countOccurrences(const std::string& value, const std::string& token)
    {
        int count = 0;
        size_t pos = value.find(token);
        while (pos != std::string::npos) {
            ++count;
            pos = value.find(token, pos + token.size());
        }
        return count;
    }
    //=============================================================================
} // namespace CodeAnalyzerAst
//=============================================================================
} // namespace Nelson
//=============================================================================
