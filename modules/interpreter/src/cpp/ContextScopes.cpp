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
#include "Context.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "RecursionStack.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
Context::pushScope(const std::string& name)
{
    if (scopestack.size() > getRecursionDepth()) {
        Error(L"Nelson:stackDepthExceeded", L"Allowable stack depth exceeded...");
    }
    try {
        scopestack.emplace_back(new Scope(name));
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:stackDepthExceeded", L"Allowable stack depth exceeded...");
    }
}
//=============================================================================
void
Context::pushScope(const std::string& name, const std::wstring& fullfilename)
{
    Scope* sc = nullptr;
    if (scopestack.size() > getRecursionDepth()) {
        Error(L"Nelson:stackDepthExceeded", L"Allowable stack depth exceeded...");
    }
    try {
        if (!scopestack.empty()) {
            if (scopestack.back()->getName() == name
                && scopestack.back()->getFilename() == fullfilename) {
                sc = new Scope(*scopestack.back());
            } else {
                sc = new Scope(name, fullfilename);
            }
        } else {
            sc = new Scope(name, fullfilename);
        }
    } catch (const std::bad_alloc&) {
        Error(L"Nelson:stackDepthExceeded", L"Allowable stack depth exceeded...");
    }
    scopestack.emplace_back(sc);
}
//=============================================================================
void
Context::popScope()
{
    if (scopestack.size() == 1) {
        Error(L"Nelson:interpreter:cannotPopGlobalScope",
            L"Attempt to pop global scope off of context stack!");
    }
    Scope* sc = scopestack.back();
    scopestack.pop_back();
    delete sc;
}
//=============================================================================
void
Context::bypassScope(int count)
{
    if (count < 0) {
        count = (int)(scopestack.size() - 2);
    }
    if (count >= (int)scopestack.size()) {
        return;
    }
    for (int i = 0; i < count; i++) {
        bypassstack.push_back(scopestack.back());
        scopestack.pop_back();
    }
}
//=============================================================================
void
Context::restoreBypassedScopes()
{
    std::reverse(bypassstack.begin(), bypassstack.end());
    for (auto* scope : bypassstack) {
        scopestack.push_back(scope);
    }
    bypassstack.clear();
}
//=============================================================================
void
Context::restoreBypassedScopes(int count)
{
    if (count <= 0 || bypassstack.empty()) {
        return;
    }
    int toRestore = std::min(count, static_cast<int>(bypassstack.size()));
    for (int i = 0; i < toRestore; ++i) {
        scopestack.push_back(bypassstack.back());
        bypassstack.pop_back();
    }
}
//=============================================================================
Scope*
Context::getCallerScope()
{
    if (scopestack.size() < 2) {
        return scopestack.empty() ? nullptr : scopestack[0];
    }
    return scopestack[scopestack.size() - 2];
}
//=============================================================================
Scope*
Context::getBaseScope()
{
    return scopestack.size() > 1 ? scopestack[1] : nullptr;
}
//=============================================================================
stringVector
Context::getLockedVariables()
{
    return scopestack.back()->getLockedVariables();
}
//=============================================================================
bool
Context::isLockedVariable(const std::string& varname)
{
    return scopestack.back()->isLockedVariable(varname);
}
//=============================================================================
bool
Context::lockVariable(const std::string& varname)
{
    return scopestack.back()->lockVariable(varname);
}
//=============================================================================
bool
Context::unlockVariable(const std::string& varname)
{
    return scopestack.back()->unlockVariable(varname);
}
//=============================================================================
size_t
Context::getRecursionDepth()
{
    return currentrecursiondepth;
}
//=============================================================================
bool
Context::setRecursionDepth(size_t newDepth)
{
    if (newDepth <= MAX_RECURSION_FUNCTION_CALL) {
        currentrecursiondepth = newDepth;
        return true;
    }
    return false;
}
//=============================================================================
size_t
Context::maximumRecursionDepth()
{
    return MAX_RECURSION_FUNCTION_CALL;
}
//=============================================================================
std::string
Context::getMangledPrivateFunction(const std::wstring& directory, const std::string& functionName)
{
    return wstring_to_utf8(directory) + "/private/" + functionName;
}
//=============================================================================
bool
Context::isInMacroFile()
{
    Scope* scope = getCurrentScope();
    if (scope) {
        return scope->isInMacroFile();
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
