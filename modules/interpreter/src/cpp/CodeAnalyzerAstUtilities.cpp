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
        return value.size() == word.size() || !isIdentifierChar(value[word.size()]);
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
    int
    inlineBlockBalance(const std::string& line)
    {
        static const std::set<std::string> openers
            = { "if", "for", "parfor", "while", "switch", "try", "function", "spmd" };
        const std::string code = codeWithoutStringsAndComments(line);
        int balance = 0;
        int depth = 0;
        for (size_t k = 0; k < code.size();) {
            const char c = code[k];
            if (c == '(' || c == '[' || c == '{') {
                ++depth;
            } else if ((c == ')' || c == ']' || c == '}') && depth > 0) {
                --depth;
            }
            if (!isIdentifierStart(c)) {
                ++k;
                continue;
            }
            const size_t first = k++;
            while (k < code.size() && isIdentifierChar(code[k])) {
                ++k;
            }
            if (depth > 0 || (first > 0 && code[first - 1] == '.')) {
                continue;
            }
            const std::string word = code.substr(first, k - first);
            if (openers.count(word) != 0) {
                ++balance;
            } else if (word == "end") {
                --balance;
            }
        }
        return balance;
    }
    //=============================================================================
    size_t
    firstStatementEnd(const std::string& code, size_t cursor)
    {
        int depth = 0;
        for (size_t k = cursor; k < code.size(); ++k) {
            const char c = code[k];
            if (c == '(' || c == '[' || c == '{') {
                ++depth;
            } else if ((c == ')' || c == ']' || c == '}') && depth > 0) {
                --depth;
            } else if ((c == ',' || c == ';') && depth == 0) {
                return k;
            }
        }
        return code.size();
    }
    //=============================================================================
    bool
    blockHeaderHasStatement(const std::string& line)
    {
        const std::string code = codeWithoutStringsAndComments(line);
        const size_t end = firstStatementEnd(code, 0);
        return end < code.size() && !trim(code.substr(end + 1)).empty();
    }
    //=============================================================================
    bool
    isArgumentsBlockHeader(const std::string& line)
    {
        // Keywords are case sensitive: a property named Arguments is not a block.
        const std::string code = trim(codeWithoutStringsAndComments(line));
        if (!startsWithWord(code, "arguments")) {
            return false;
        }
        const std::string rest = trim(code.substr(9));
        if (rest.empty() || rest == ";" || rest == ",") {
            return true;
        }
        if (rest.front() != '(' || rest.find(')') == std::string::npos) {
            return false;
        }
        const std::string attribute = toLowerAscii(trim(rest.substr(1, rest.find(')') - 1)));
        const std::string after = trim(rest.substr(rest.find(')') + 1));
        return (attribute == "input" || attribute == "output" || attribute == "repeating"
                   || attribute == "input, repeating" || attribute == "output, repeating")
            && (after.empty() || after == ";" || after == ",");
    }
    //=============================================================================
} // namespace CodeAnalyzerAst
//=============================================================================
} // namespace Nelson
//=============================================================================
