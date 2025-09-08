//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
// Copyright (c) 2002, 2003 Samit Basu
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "Context.hpp"
#include "ArrayOf.hpp"
#include "RecursionStack.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Context::Context()
{
    bypassstack.reserve(1024);
    currentrecursiondepth = DEFAULT_RECURSION_FUNCTION_CALL;
    scopestack.reserve(DEFAULT_RECURSION_FUNCTION_CALL);
    pushScope("global");
    pushScope("base");
}
//=============================================================================
Context::~Context()
{
    while (!scopestack.empty()) {
        delete scopestack.back();
        scopestack.pop_back();
    }
}
//=============================================================================
Scope*
Context::getCurrentScope()
{
    return scopestack.empty() ? nullptr : scopestack.back();
}
//=============================================================================
Scope*
Context::getGlobalScope()
{
    return scopestack.empty() ? nullptr : scopestack.front();
}
//=============================================================================
void
Context::pushScope(const std::string& name)
{
    if (scopestack.size() > getRecursionDepth()) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    try {
        scopestack.emplace_back(new Scope(name));
    } catch (const std::bad_alloc&) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
}
//=============================================================================
void
Context::pushScope(const std::string& name, const std::wstring& fullfilename)
{
    Scope* sc = nullptr;
    if (scopestack.size() > getRecursionDepth()) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
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
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    scopestack.emplace_back(sc);
}
//=============================================================================
void
Context::popScope()
{
    if (scopestack.size() == 1) {
        Error(ERROR_POP_GLOBAL_SCOPE);
    }
    delete scopestack.back();
    scopestack.pop_back();
}
//=============================================================================
void
Context::insertVariableLocally(const std::string& varName, const ArrayOf& var)
{
    scopestack.back()->insertVariable(varName, var);
}
//=============================================================================
bool
Context::insertVariable(const std::string& varName, const ArrayOf& var)
{
    Scope* active = nullptr;
    std::string mapName;
    Scope* backScope = scopestack.back();
    Scope* frontScope = scopestack.front();
    if (backScope->isVariablePersistent(varName)) {
        mapName = backScope->getMangledName(varName);
        active = frontScope;
    } else if (backScope->isVariableGlobal(varName)) {
        mapName = varName;
        active = frontScope;
    } else {
        return backScope->insertVariable(varName, var);
    }
    return active->insertVariable(mapName, var);
}
//=============================================================================
ArrayOf*
Context::lookupVariable(const std::string& varName)
{
    Scope* active;
    std::string mapName;
    Scope* back = scopestack.back();
    if (back->isVariablePersistent(varName)) {
        mapName = back->getMangledName(varName);
        active = scopestack.front();
    } else if (back->isVariableGlobal(varName)) {
        mapName = varName;
        active = scopestack.front();
    } else {
        return back->lookupVariable(varName);
    }
    return active->lookupVariable(mapName);
}
//=============================================================================
bool
Context::lookupVariable(const std::string& varName, ArrayOf& var)
{
    ArrayOf* res = lookupVariable(varName);
    if (res != nullptr) {
        var = res[0];
        return true;
    }
    return false;
}
//=============================================================================
bool
Context::isVariable(const std::wstring& varname)
{
    return isVariable(wstring_to_utf8(varname));
}
//=============================================================================
bool
Context::isVariable(const std::string& varname)
{
    Scope* active;
    Scope* back = scopestack.back();
    if (back->isVariablePersistent(varname)) {
        /*std::string mapName = */ back->getMangledName(varname);
        active = scopestack.front();
    } else if (back->isVariableGlobal(varname)) {
        active = scopestack.front();
    } else {
        return back->isVariable(varname);
    }
    return active->isVariable(varname);
}
//=============================================================================
bool
Context::isVariableGlobal(const std::string& varName)
{
    return scopestack.back()->isVariableGlobal(varName);
}
//=============================================================================
bool
Context::isVariablePersistent(const std::string& varName)
{
    return scopestack.back()->isVariablePersistent(varName);
}
//=============================================================================
bool
Context::lookupVariableLocally(const std::string& varName, ArrayOf& var)
{
    return scopestack.back()->lookupVariable(varName, var);
}
//=============================================================================
void
Context::insertMacroFunctionLocally(FunctionDefPtr f)
{
    scopestack.back()->insertMacroFunctionLocally(f);
}
//=============================================================================
bool
Context::lookupFunction(const std::wstring& wfuncName, FunctionDefPtr& val, bool builtinOnly)
{
    std::string funcName = wstring_to_utf8(wfuncName);
    return lookupFunction(funcName, val, builtinOnly);
}
//=============================================================================
bool
Context::lookupFunction(const std::string& funcName, FunctionDefPtr& val, bool builtinOnly)
{
    bool found = false;
    if (builtinOnly) {
        if (FunctionsInMemory::getInstance()->find(
                funcName, val, FunctionsInMemory::FIND_FUNCTION_TYPE::BUILTIN)) {
            return true;
        }
        found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
        if (found) {
            FunctionsInMemory::getInstance()->add(funcName, val);
        }
        return found;
    }

    if (FunctionsInMemory::getInstance()->find(funcName, val)) {
        if (val->type() == NLS_MACRO_FUNCTION || val->type() == NLS_MEX_FUNCTION) {
            PathFunctionIndexerManager::getInstance()->hashOnFileWatcher();
        }
        return true;
    } else {
        PathFunctionIndexerManager::getInstance()->hashOnFileWatcher();
    }

    if (scopestack.back()->lookupFunction(funcName, val)) {
        return true;
    }
    found = PathFunctionIndexerManager::getInstance()->find(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }

    if (isInMacroFile()) {
        std::wstring directory = scopestack.back()->getDirectory();
        found = PathFunctionIndexerManager::getInstance()->find(
            getMangledPrivateFunction(directory, funcName), val);
        if (found) {
            FunctionsInMemory::getInstance()->add(
                getMangledPrivateFunction(directory, funcName), val);
            return true;
        }
    }

    found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }
    return scopestack.front()->lookupFunction(funcName, val);
}
//=============================================================================
void
Context::printMe()
{
}
//=============================================================================
void
Context::enterLoop()
{
    scopestack.back()->enterLoop();
}
//=============================================================================
void
Context::exitLoop()
{
    scopestack.back()->exitLoop();
}
//=============================================================================
bool
Context::inLoop()
{
    return scopestack.back()->inLoop();
}
//=============================================================================
void
Context::addPersistentVariable(const std::string& var)
{
    Scope* back = scopestack.back();
    Scope* front = scopestack.front();
    // Delete local variables with this name
    back->deleteVariable(var);
    // Delete global variables with this name
    front->deleteVariable(var);
    back->addPersistentVariablePointer(var);
    ArrayOf v;
    if (!front->lookupVariable(back->getMangledName(var), v)) {
        front->insertVariable(back->getMangledName(var), ArrayOf::emptyConstructor());
    }
}
//=============================================================================
void
Context::addGlobalVariable(const std::string& var)
{
    Scope* back = scopestack.back();
    Scope* front = scopestack.front();
    // Delete local variables with this name
    back->deleteVariable(var);
    // Delete global persistent variables with this name
    front->deleteVariable(back->getMangledName(var));
    // Add a point in the local scope to the global variable
    back->addGlobalVariablePointer(var);
    // Make sure the variable exists
    ArrayOf v;
    if (!front->lookupVariable(var, v)) {
        front->insertVariable(var, ArrayOf::emptyConstructor());
    }
}
//=============================================================================
void
Context::deleteVariable(const std::string& var)
{
    if (isVariableGlobal(var)) {
        scopestack.front()->deleteVariable(var);
        scopestack.back()->deleteGlobalVariablePointer(var);
        return;
    }
    if (isVariablePersistent(var)) {
        scopestack.front()->deleteVariable(scopestack.back()->getMangledName(var));
        scopestack.back()->deletePersistentVariablePointer(var);
        return;
    }
    scopestack.back()->deleteVariable(var);
}
//=============================================================================
void
Context::bypassScope(int count)
{
    if (count < 0) {
        count = (int)(scopestack.size() - 2);
    }
    if (count > (int)scopestack.size()) {
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
Scope*
Context::getCallerScope()
{
    Scope* callerScope;
    if (scopestack.size() - 2 <= 0) {
        callerScope = scopestack[1];
    } else {
        callerScope = scopestack[scopestack.size() - 2];
    }
    return callerScope;
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
Context::getMaximumRecursionDepth()
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
