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
#include "Scope.hpp"
#include "ArrayOf.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFuncManager.hpp"
#include <algorithm>
#include <stdio.h>
//=============================================================================
namespace Nelson {
//=============================================================================
static boost::unordered_map<std::string, FuncPtr> cachedFunc;
//=============================================================================
Scope::Scope(std::string scopeName)
{
    name = scopeName;
    loopLevel = 0;
    cachedFunc.clear();
}
//=============================================================================
Scope::~Scope() {}
//=============================================================================
void
Scope::insertMacroFunctionLocally(FuncPtr a)
{
    currentLocalFunctions.add(a->name, a);
}
//=============================================================================
bool
Scope::deleteBuiltin(void* fptr)
{
    cachedFunc.clear();
    return BuiltInFunctionDefManager::getInstance()->remove((BuiltInFuncPtr)fptr);
}
//=============================================================================
void
Scope::deleteFunction(const std::string funcName)
{
    cachedFunc.clear();
    BuiltInFunctionDefManager::getInstance()->remove(funcName);
}
//=============================================================================
bool
Scope::lookupFunction(std::string funcName, FuncPtr& val, bool builtinOnly)
{
    boost::unordered_map<std::string, FuncPtr>::const_iterator foundit = cachedFunc.find(funcName);
    if (foundit != cachedFunc.end()) {
        val = foundit->second;
        return true;
    }
    bool found = false;
    if (builtinOnly) {
        found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
        if (found) {
            cachedFunc.emplace(funcName, val);
        }
        return found;
    } else {
        found = currentLocalFunctions.find(funcName, val);
        if (found) {
            cachedFunc.emplace(funcName, val);
            return true;
        }
        found = PathFuncManager::getInstance()->find(funcName, val);
        if (found) {
            cachedFunc.emplace(funcName, val);
            return true;
        }
        found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
        if (found) {
            cachedFunc.emplace(funcName, val);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
Scope::isPointerOnFunction(FuncPtr val)
{
    if (PathFuncManager::getInstance()->isPointerOnPathFunctionDef(val)) {
        return true;
    }
    if (BuiltInFunctionDefManager::getInstance()->isPointerOnBuiltInFunctionDef(val)) {
        return true;
    }
    return false;
}
//=============================================================================
bool
Scope::lookupVariable(const std::string& varName, ArrayOf& val)
{
    return variablesTab.findVariable(varName, val);
}
//=============================================================================
std::string
Scope::getName()
{
    return name;
}
//=============================================================================
bool
Scope::insertVariable(const std::string& varName, const ArrayOf& var)
{
    return variablesTab.insertVariable(varName, var);
}
//=============================================================================
void
Scope::printMe()
{}
//=============================================================================
void
Scope::printData()
{}
//=============================================================================
void
Scope::enterLoop()
{
    loopLevel++;
}
//=============================================================================
void
Scope::exitLoop()
{
    loopLevel--;
}
//=============================================================================
bool
Scope::inLoop()
{
    return (loopLevel > 0);
}
//=============================================================================
void
Scope::addGlobalVariablePointer(std::string varName)
{
    if (!isVariableGlobal(varName)) {
        globalVars.push_back(varName);
    }
}
//=============================================================================
void
Scope::deleteGlobalVariablePointer(std::string varName)
{
    stringVector::iterator i = std::find(globalVars.begin(), globalVars.end(), varName);
    if (*i == varName) {
        globalVars.erase(i);
    }
}
//=============================================================================
bool
Scope::isVariableGlobal(const std::string& varName)
{
    bool foundName = false;
    size_t i = 0;
    if (globalVars.empty()) {
        return false;
    }
    while (i < globalVars.size() && !foundName) {
        foundName = (globalVars[i] == varName);
        if (!foundName) {
            i++;
        }
    }
    return foundName;
}
//=============================================================================
void
Scope::addPersistentVariablePointer(std::string varName)
{
    if (!isVariablePersistent(varName)) {
        persistentVars.push_back(varName);
    }
}
//=============================================================================
void
Scope::deletePersistentVariablePointer(std::string varName)
{
    stringVector::iterator i = std::find(persistentVars.begin(), persistentVars.end(), varName);
    if (i != persistentVars.end()) {
        persistentVars.erase(i);
    }
}
//=============================================================================
bool
Scope::isVariablePersistent(const std::string& varName)
{
    bool foundName = false;
    size_t i = 0;
    if (persistentVars.empty()) {
        return false;
    }
    while (i < persistentVars.size() && !foundName) {
        foundName = (persistentVars[i] == varName);
        if (!foundName) {
            i++;
        }
    }
    return foundName;
}
//=============================================================================
std::string
Scope::getMangledName(std::string varName)
{
    return (std::string("_") + name + std::string("_") + varName);
}
//=============================================================================
bool
Scope::deleteVariable(std::string var)
{
    return variablesTab.deleteVariable(var);
}
//=============================================================================
stringVector
Scope::getLockedVariables()
{
    return variablesTab.getLockedVariables();
}
//=============================================================================
bool
Scope::isLockedVariable(std::string varname)
{
    return variablesTab.isLockedVariable(varname);
}
//=============================================================================
bool
Scope::lockVariable(std::string varname)
{
    return variablesTab.lockVariable(varname);
}
//=============================================================================
bool
Scope::unlockVariable(std::string varname)
{
    return variablesTab.unlockVariable(varname);
}
//=============================================================================
stringVector
Scope::getVariablesList(bool withPersistent)
{
    return variablesTab.getVariablesList(withPersistent);
}
//=============================================================================
stringVector
Scope::getBuiltinsList()
{
    return BuiltInFunctionDefManager::getInstance()->getNameList();
}
//=============================================================================
void
Scope::setNargIn(int _nargin)
{
    this->nargin = _nargin;
}
//=============================================================================
int
Scope::getNargIn()
{
    return this->nargin;
}
//=============================================================================
void
Scope::setNargOut(int _nargout)
{
    this->nargout = _nargout;
}
//=============================================================================
int
Scope::getNargOut()
{
    return this->nargout;
}
//=============================================================================
bool
Scope::isVariable(std::string varname)
{
    return variablesTab.isVariable(varname);
}
//=============================================================================
}
//=============================================================================
