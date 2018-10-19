//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
#include "Context.hpp"
#include "ArrayOf.hpp"
#include "RecursionStack.hpp"
#include "characters_encoding.hpp"
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
Context::pushScope(std::string name)
{
    Scope* sc = nullptr;
    if (scopestack.size() > getRecursionDepth()) {
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    try {
        sc = new Scope(name);
    } catch (const std::bad_alloc& e) {
        e.what();
        Error(ERROR_STACK_DEPTH_EXCEEDED);
    }
    if (sc) {
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
Context::insertVariableLocally(std::string varName, const ArrayOf& var)
{
    scopestack.back()->insertVariable(varName, var);
}
//=============================================================================
bool
Context::insertVariable(const std::string& varName, const ArrayOf& var)
{
    Scope* active;
    std::string mapName;
    if (scopestack.back()->isVariablePersistent(varName)) {
        mapName = scopestack.back()->getMangledName(varName);
        active = scopestack.front();
    } else if (scopestack.back()->isVariableGlobal(varName)) {
        mapName = varName;
        active = scopestack.front();
    } else {
        return scopestack.back()->insertVariable(varName, var);
    }
    return active->insertVariable(mapName, var);
}
//=============================================================================
bool
Context::lookupVariable(const std::string& varName, ArrayOf& var)
{
    Scope* active;
    std::string mapName;
    if (scopestack.back()->isVariablePersistent(varName)) {
        mapName = scopestack.back()->getMangledName(varName);
        active = scopestack.front();
    } else if (scopestack.back()->isVariableGlobal(varName)) {
        mapName = varName;
        active = scopestack.front();
    } else {
        return scopestack.back()->lookupVariable(varName, var);
    }
    return active->lookupVariable(mapName, var);
}
//=============================================================================
bool
Context::isVariable(std::string varname)
{
    Scope* active;
    if (scopestack.back()->isVariablePersistent(varname)) {
        /*std::string mapName = */ scopestack.back()->getMangledName(varname);
        active = scopestack.front();
    } else if (scopestack.back()->isVariableGlobal(varname)) {
        active = scopestack.front();
    } else {
        return scopestack.back()->isVariable(varname);
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
Context::lookupVariableLocally(std::string varName, ArrayOf& var)
{
    return scopestack.back()->lookupVariable(varName, var);
}
//=============================================================================
void
Context::insertMacroFunctionLocally(FuncPtr f)
{
    scopestack.back()->insertMacroFunctionLocally(f);
}
//=============================================================================
bool
Context::lookupFunction(const std::wstring& wfuncName, FuncPtr& val, bool builtinOnly)
{
    std::string funcName = wstring_to_utf8(wfuncName);
    return lookupFunction(funcName, val, builtinOnly);
}
//=============================================================================
bool
Context::lookupFunction(const std::string& funcName, FuncPtr& val, bool builtinOnly)
{
    if (scopestack.back()->lookupFunction(funcName, val, builtinOnly)) {
        return true;
    }
    return lookupFunctionGlobally(funcName, val, false);
}
//=============================================================================
bool
Context::lookupFunctionGlobally(const std::string& funcName, FuncPtr& val, bool builtinOnly)
{
    return scopestack.front()->lookupFunction(funcName, val, builtinOnly);
}
//=============================================================================
void
Context::deleteFunctionGlobally(const std::string& funcName)
{
    scopestack.front()->deleteFunction(funcName);
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
Context::addPersistentVariable(std::string var)
{
    // Delete local variables with this name
    scopestack.back()->deleteVariable(var);
    // Delete global variables with this name
    scopestack.front()->deleteVariable(var);
    scopestack.back()->addPersistentVariablePointer(var);
    ArrayOf v;
    if (!scopestack.front()->lookupVariable(scopestack.back()->getMangledName(var), v)) {
        scopestack.front()->insertVariable(
            scopestack.back()->getMangledName(var), ArrayOf::emptyConstructor());
    }
}
//=============================================================================
void
Context::addGlobalVariable(std::string var)
{
    // Delete local variables with this name
    scopestack.back()->deleteVariable(var);
    // Delete global persistent variables with this name
    scopestack.front()->deleteVariable(scopestack.back()->getMangledName(var));
    // Add a point in the local scope to the global variable
    scopestack.back()->addGlobalVariablePointer(var);
    // Make sure the variable exists
    ArrayOf v;
    if (!scopestack.front()->lookupVariable(var, v)) {
        scopestack.front()->insertVariable(var, ArrayOf::emptyConstructor());
    }
}
//=============================================================================
void
Context::deleteVariable(std::string var)
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
        count = (int)scopestack.size();
    }
    while ((count > 0) && (scopestack.back()->getName() != "base")) {
        bypassstack.push_back(scopestack.back());
        scopestack.pop_back();
        count--;
    }
}
//=============================================================================
void
Context::restoreBypassedScopes()
{
    for (size_t i = 0; i < bypassstack.size(); i++) {
        scopestack.push_back(bypassstack[i]);
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
Context::isLockedVariable(std::string varname)
{
    return scopestack.back()->isLockedVariable(varname);
}
//=============================================================================
bool
Context::lockVariable(std::string varname)
{
    return scopestack.back()->lockVariable(varname);
}
//=============================================================================
bool
Context::unlockVariable(std::string varname)
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
}
//=============================================================================
