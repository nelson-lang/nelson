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
#include "AnonymousCapturedNames.hpp"
#include "MacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isIdentifierStart(char c);
static bool
isIdentifierChar(char c);
static bool
isDigit(const std::string& content, size_t k);
static bool
quoteStartsText(const std::string& content, size_t k);
static size_t
skipQuotedText(const std::string& content, size_t k);
static size_t
skipNumber(const std::string& content, size_t k);
//=============================================================================
std::unordered_set<std::string>
anonymousReferencedNames(const std::string& content, const stringVector& arguments)
{
    std::unordered_set<std::string> names;
    for (size_t k = 0; k < content.size();) {
        char c = content[k];
        if (c == '"' || (c == '\'' && quoteStartsText(content, k))) {
            k = skipQuotedText(content, k);
            continue;
        }
        if (isDigit(content, k)) {
            k = skipNumber(content, k);
            continue;
        }
        if (!isIdentifierStart(c)) {
            ++k;
            continue;
        }
        size_t start = k++;
        while (k < content.size() && isIdentifierChar(content[k])) {
            ++k;
        }
        names.insert(content.substr(start, k - start));
    }
    for (const std::string& argument : arguments) {
        names.erase(argument);
    }
    return names;
}
//=============================================================================
bool
anonymousContentMayNeedFunctionScope(
    const std::unordered_set<std::string>& referencedNames, const stringVector& captures)
{
    for (const std::string& name : referencedNames) {
        if (std::find(captures.begin(), captures.end(), name) == captures.end()) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
anonymousReferencesNestedFunction(const std::unordered_set<std::string>& referencedNames,
    const std::vector<std::pair<std::string, FunctionDefPtr>>& localFunctions)
{
    for (const auto& localFunction : localFunctions) {
        FunctionDef* functionDef = localFunction.second;
        if (functionDef == nullptr || functionDef->type() != NLS_MACRO_FUNCTION
            || referencedNames.find(localFunction.first) == referencedNames.end()) {
            continue;
        }
        if (static_cast<MacroFunctionDef*>(functionDef)->nestedFunction) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
isIdentifierStart(char c)
{
    unsigned char uc = static_cast<unsigned char>(c);
    return std::isalpha(uc) || c == '_';
}
//=============================================================================
static bool
isIdentifierChar(char c)
{
    unsigned char uc = static_cast<unsigned char>(c);
    return std::isalnum(uc) || c == '_';
}
//=============================================================================
static bool
isDigit(const std::string& content, size_t k)
{
    return k < content.size() && std::isdigit(static_cast<unsigned char>(content[k]));
}
//=============================================================================
// A single quote opens text unless it follows an operand, where it is a
// transpose. When in doubt (blank before the quote) it is read as a transpose:
// text mistaken for code only adds names, code mistaken for text would drop
// some.
static bool
quoteStartsText(const std::string& content, size_t k)
{
    while (k > 0) {
        --k;
        char previous = content[k];
        if (std::isspace(static_cast<unsigned char>(previous))) {
            continue;
        }
        return !(isIdentifierChar(previous) || previous == ')' || previous == ']' || previous == '}'
            || previous == '\'' || previous == '.');
    }
    return true;
}
//=============================================================================
// k is at the opening quote; returns the index after the closing quote, a
// doubled quote being an escaped one.
static size_t
skipQuotedText(const std::string& content, size_t k)
{
    char quote = content[k];
    ++k;
    while (k < content.size()) {
        if (content[k] == quote) {
            if (k + 1 < content.size() && content[k + 1] == quote) {
                k += 2;
                continue;
            }
            return k + 1;
        }
        ++k;
    }
    return k;
}
//=============================================================================
// k is at a digit; returns the index after the numeric literal so that its
// exponent or imaginary suffix is not read as an identifier.
static size_t
skipNumber(const std::string& content, size_t k)
{
    while (isDigit(content, k)) {
        ++k;
    }
    if (k < content.size() && content[k] == '.' && isDigit(content, k + 1)) {
        ++k;
        while (isDigit(content, k)) {
            ++k;
        }
    }
    if (k < content.size() && (content[k] == 'e' || content[k] == 'E')) {
        size_t exponent = k + 1;
        if (exponent < content.size() && (content[exponent] == '+' || content[exponent] == '-')) {
            ++exponent;
        }
        if (isDigit(content, exponent)) {
            k = exponent;
            while (isDigit(content, k)) {
                ++k;
            }
        }
    }
    if (k < content.size() && (content[k] == 'i' || content[k] == 'j')
        && !(k + 1 < content.size() && isIdentifierChar(content[k + 1]))) {
        ++k;
    }
    return k;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
