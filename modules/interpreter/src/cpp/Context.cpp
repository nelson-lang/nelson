//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
// Copyright (c) 2002, 2003 Samit Basu
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//=============================================================================
#include <boost/format.hpp>
#include "Context.hpp"
#include "ArrayOf.hpp"
#include "RecursionStack.hpp"
#include "characters_encoding.hpp"
#include "FunctionsInMemory.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFuncManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Context::Context()
{
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
    return scopestack.back();
}
//=============================================================================
Scope*
Context::getGlobalScope()
{
    return scopestack.front();
}
//=============================================================================
void
Context::pushScope(const std::string& name)
{
    Scope* sc = nullptr;
    if (scopestack.size() > getRecursionDepth()) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    try {
        sc = new Scope(name);
    } catch (const std::bad_alloc&) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    if (sc != nullptr) {
        scopestack.push_back(sc);
    } else {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
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

    FunctionDefPtr functionDefInMem = nullptr;

    if (FunctionsInMemory::getInstance()->find(funcName, val)) {
        /*
        if (val->type() == NLS_MACRO_FUNCTION || val->type() == NLS_MEX_FUNCTION) {
            std::wstring pathname = val->getPath();
            if (PathFuncManager::getInstance()->isAvailablePath(pathname)) {
                return true;
            } else {
                functionDefInMem = val;
                val = nullptr;
            }
        } else {
            return true;
        }
        */
        return true;
    }

    if (scopestack.back()->lookupFunction(funcName, val)) {
        return true;
    }

    found = PathFuncManager::getInstance()->find(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }
    found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }
    bool res = scopestack.front()->lookupFunction(funcName, val);
    if (!res && functionDefInMem != nullptr) {
        std::string utf8msg = str(boost::format(_("'%s' is not found in the current folder or on "
                                                  "the Nelson path, but exists in:"))
            % funcName);
        std::wstring msg = utf8_to_wstring(utf8msg) + L"\n" + functionDefInMem->getPath();
        Error(msg);
    }
    return res;
}
//=============================================================================
void
Context::printMe()
{}
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
    for (size_t i = 0; i < bypassstack.size(); i++) {
        scopestack.push_back(bypassstack[bypassstack.size() - 1 - i]);
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
    Scope* baseScope;
    baseScope = scopestack[1];
    return baseScope;
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
} // namespace Nelson
//=============================================================================
