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
#include <algorithm>
#include <cstdio>
#include "Scope.hpp"
#include "ArrayOf.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "OverloadName.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
Scope::Scope(const Scope& scopeOrigin)
    : name(scopeOrigin.name)
    , loopLevel(0)
    , filename(scopeOrigin.filename)
    , isInMacro(scopeOrigin.isInMacro)
    , directory(scopeOrigin.directory)
    , importList(scopeOrigin.importList)
{
}
//=============================================================================
Scope::Scope(const std::string& scopeName)
    : name(scopeName), loopLevel(0), filename(L""), directory(L""), isInMacro(false)
{
}
//=============================================================================
Scope::Scope(const std::string& scopeName, const std::wstring& fullfilename)
    : name(scopeName)
    , loopLevel(0)
    , filename(fullfilename)
    , isInMacro(StringHelpers::ends_with(fullfilename, L".m")
          || StringHelpers::ends_with(fullfilename, L".nbc"))
{
    FileSystemWrapper::Path fullPath(fullfilename);
    directory = fullPath.parent_path().generic_wstring();
    std::wstring privateStr = L"/private";
    if (StringHelpers::ends_with(directory, privateStr)) {
        directory.erase(directory.size() - privateStr.size(), privateStr.size());
    }
}
//=============================================================================
Scope::~Scope()
{
    name.clear();
    loopLevel = 0;
}
//=============================================================================
void
Scope::insertMacroFunctionLocally(FunctionDefPtr a)
{
    currentLocalFunctions.add(a->getName(), a);
}
//=============================================================================
void
Scope::removeMacroFunctionLocally(const std::string& funcName)
{
    currentLocalFunctions.remove(funcName);
}
//=============================================================================
bool
Scope::lookupFunction(const std::string& funcName, FunctionDefPtr& val)
{
    if (funcName[0] == OVERLOAD_SYMBOL_CHAR) {
        return false;
    }
    return currentLocalFunctions.find(funcName, val);
}
//=============================================================================
std::vector<std::pair<std::string, FunctionDefPtr>>
Scope::getLocalFunctionsList()
{
    return currentLocalFunctions.getAll();
}
//=============================================================================
ArrayOf*
Scope::lookupVariable(const std::string& varName)
{
    return variablesTab.findVariable(varName);
}
//=============================================================================
bool
Scope::lookupVariable(const std::string& varName, ArrayOf& val)
{
    return variablesTab.findVariable(varName, val);
}
//=============================================================================
void
Scope::clearVariables()
{
    variablesTab.clearVariables();
}
//=============================================================================
bool
Scope::insertVariable(const std::string& varName, const ArrayOf& var)
{
    return variablesTab.insertVariable(varName, var);
}
//=============================================================================
bool
Scope::insertVariableWithoutNameValidation(const std::string& varName, const ArrayOf& var)
{
    return variablesTab.insertVariableWithoutNameValidation(varName, var);
}
//=============================================================================
void
Scope::printMe()
{
}
//=============================================================================
void
Scope::printData()
{
}
//=============================================================================
void
Scope::addGlobalVariablePointer(const std::string& varName)
{
    globalVars.insert(varName);
}
//=============================================================================
void
Scope::deleteGlobalVariablePointer(const std::string& varName)
{
    globalVars.erase(varName);
}
//=============================================================================
bool
Scope::isVariableGlobal(const std::string& varName)
{
    return globalVars.find(varName) != globalVars.end();
}
//=============================================================================
void
Scope::addPersistentVariablePointer(const std::string& varName)
{
    persistentVars.insert(varName);
}
//=============================================================================
void
Scope::deletePersistentVariablePointer(const std::string& varName)
{
    persistentVars.erase(varName);
}
//=============================================================================
bool
Scope::isVariablePersistent(const std::string& varName)
{
    return persistentVars.find(varName) != persistentVars.end();
}
//=============================================================================
std::string
Scope::getMangledName(const std::string& varName)
{
    return (std::string("_") + name + std::string("_") + varName);
}
//=============================================================================
bool
Scope::deleteVariable(const std::string& var)
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
Scope::isLockedVariable(const std::string& varname)
{
    return variablesTab.isLockedVariable(varname);
}
//=============================================================================
bool
Scope::lockVariable(const std::string& varname)
{
    return variablesTab.lockVariable(varname);
}
//=============================================================================
bool
Scope::unlockVariable(const std::string& varname)
{
    return variablesTab.unlockVariable(varname);
}
//=============================================================================
void
Scope::getVariablesList(bool withPersistent, stringVector& list)
{
    list.clear();
    list = variablesTab.getVariablesList(withPersistent);
}
//=============================================================================
void
Scope::getVariablesList(bool withPersistent, wstringVector& list)
{
    const stringVector& ulist = variablesTab.getVariablesList(withPersistent);
    list.clear();
    list.reserve(ulist.size());
    for (const auto& k : ulist) {
        list.emplace_back(utf8_to_wstring(k));
    }
}
//=============================================================================
void
Scope::setInputArgumentNames(const stringVector& names)
{
    inputNames = names;
}
//=============================================================================
stringVector
Scope::getInputArgumentNames()
{
    return inputNames;
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
Scope::isVariable(const std::string& varname)
{
    return variablesTab.isVariable(varname);
}
//=============================================================================
std::wstring
Scope::getFilename()
{
    return filename;
}
//=============================================================================
std::wstring
Scope::getDirectory()
{
    return directory;
}
//=============================================================================
bool
Scope::isInMacroFile()
{
    return isInMacro;
}
//=============================================================================
bool
Scope::addImport(const std::string& importSpec)
{
    if (std::find(importList.begin(), importList.end(), importSpec) != importList.end()) {
        return false;
    }
    importList.push_back(importSpec);
    return true;
}
//=============================================================================
void
Scope::clearImports()
{
    importList.clear();
}
//=============================================================================
stringVector
Scope::getImports()
{
    return importList;
}
//=============================================================================
void
Scope::addCleanupTask(const ArrayOf& task)
{
    cleanupTasks.push_back(task);
}
//=============================================================================
ArrayOfVector
Scope::takeCleanupTasks()
{
    ArrayOfVector tasks = cleanupTasks;
    cleanupTasks.clear();
    return tasks;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
