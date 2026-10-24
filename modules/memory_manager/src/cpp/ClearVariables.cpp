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
#include "ClearVariables.hpp"
#include "ClearGlobal.hpp"
#include "ClearHandle.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct ClearvarsPattern
{
    bool all = true;
    bool regexp = false;
    bool wildcard = false;
    wstringVector expressions;
    std::vector<std::wregex> patterns;
};
//=============================================================================
static void
appendUnique(stringVector& names, const std::string& name)
{
    if (std::find(names.begin(), names.end(), name) == names.end()) {
        names.push_back(name);
    }
}
//=============================================================================
static bool
hasWildcard(const wstringVector& expressions)
{
    for (const auto& expression : expressions) {
        if (expression.find(L'*') != std::wstring::npos) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static std::wstring
wildcardToRegularExpression(const std::wstring& expression)
{
    static const std::wstring specialCharacters = LR"(\.^$|()[]{}+?)";
    std::wstring pattern = L"^";
    for (wchar_t character : expression) {
        if (character == L'*') {
            pattern += L".*";
        } else {
            if (specialCharacters.find(character) != std::wstring::npos) {
                pattern += L'\\';
            }
            pattern += character;
        }
    }
    pattern += L"$";
    return pattern;
}
//=============================================================================
static std::vector<std::wregex>
regularExpressionsFrom(const wstringVector& expressions, bool wildcard)
{
    std::vector<std::wregex> patterns;
    patterns.reserve(expressions.size());
    try {
        for (const auto& expression : expressions) {
            patterns.emplace_back(wildcard ? wildcardToRegularExpression(expression) : expression);
        }
    } catch (const std::regex_error&) {
        Error("Nelson:clear:invalidRegularExpression", "Invalid regular expression.");
    }
    return patterns;
}
//=============================================================================
static ClearvarsPattern
parseClearvarsPattern(const wstringVector& arguments, size_t first, size_t last, bool defaultAll)
{
    ClearvarsPattern pattern;
    pattern.all = false;
    if (first < last && arguments[first] == L"-regexp") {
        pattern.regexp = true;
        first++;
    }
    for (size_t k = first; k < last; ++k) {
        const std::wstring& expression = arguments[k];
        if (expression == L"-global") {
            Warning(L"Nelson:clear:globalFlagOrder",
                L"The -global flag must be first in the argument list.");
            continue;
        }
        if (expression == L"-except" || expression == L"-regexp") {
            Error(L"Nelson:clear:invalidOption", L"Invalid option.");
        }
        pattern.expressions.push_back(expression);
    }
    pattern.all = defaultAll && pattern.expressions.empty();
    if (!pattern.expressions.empty()) {
        pattern.wildcard = !pattern.regexp && hasWildcard(pattern.expressions);
        if (pattern.regexp || pattern.wildcard) {
            pattern.patterns = regularExpressionsFrom(pattern.expressions, pattern.wildcard);
        }
    }
    return pattern;
}
//=============================================================================
static stringVector
clearvarsCandidateNames(Evaluator* eval, bool globalOnly)
{
    stringVector names;
    Context* context = eval->getContext();
    if (globalOnly) {
        context->getGlobalScope()->getVariablesList(false, names);
    } else {
        Scope* currentScope = context->getCurrentScope();
        currentScope->getVariablesList(false, names);
        stringVector globalNames;
        context->getGlobalScope()->getVariablesList(false, globalNames);
        for (const auto& name : globalNames) {
            if (context->isVariableGlobal(name)) {
                appendUnique(names, name);
            }
        }
    }
    return names;
}
//=============================================================================
static bool
matchesClearvarsPattern(const std::string& name, const ClearvarsPattern& pattern)
{
    if (pattern.all) {
        return true;
    }
    if (pattern.expressions.empty()) {
        return false;
    }
    const std::wstring wideName = utf8_to_wstring(name);
    if (!pattern.regexp && !pattern.wildcard) {
        return std::find(pattern.expressions.begin(), pattern.expressions.end(), wideName)
            != pattern.expressions.end();
    }
    for (const auto& expression : pattern.patterns) {
        if (std::regex_search(wideName, expression)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static void
clearvarsByName(Evaluator* eval, const std::string& name, bool globalOnly)
{
    Context* context = eval->getContext();
    if (globalOnly) {
        if (context->getGlobalScope()->isLockedVariable(name)) {
            Warning("Nelson:memory_manager:variableLocked", "variable is locked: %s", name);
        }
        ClearGlobalVariable(eval, name);
        return;
    }
    Scope* currentScope = context->getCurrentScope();
    if (currentScope->isVariableGlobal(name)) {
        currentScope->deleteGlobalVariablePointer(name);
        context->notifyVariablesCleared();
        return;
    }
    if (currentScope->isLockedVariable(name)) {
        Warning("Nelson:memory_manager:variableLocked", "variable is locked: %s", name);
    }
    ClearHandle(eval, currentScope, name);
    if (currentScope->deleteVariable(name)) {
        context->notifyVariablesCleared();
    }
}
//=============================================================================
void
ClearVariables(Evaluator* eval, const wstringVector& arguments)
{
    bool globalOnly = false;
    size_t first = 0;
    if (!arguments.empty() && arguments[0] == L"-global") {
        globalOnly = true;
        first = 1;
    }
    size_t except = arguments.size();
    for (size_t k = first; k < arguments.size(); ++k) {
        if (arguments[k] == L"-except") {
            except = k;
            break;
        }
    }
    const ClearvarsPattern target = parseClearvarsPattern(arguments, first, except, true);
    ClearvarsPattern keep;
    keep.all = false;
    if (except < arguments.size()) {
        keep = parseClearvarsPattern(arguments, except + 1, arguments.size(), false);
    }

    const stringVector candidates = clearvarsCandidateNames(eval, globalOnly);
    for (const auto& name : candidates) {
        if (matchesClearvarsPattern(name, target) && !matchesClearvarsPattern(name, keep)) {
            clearvarsByName(eval, name, globalOnly);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
