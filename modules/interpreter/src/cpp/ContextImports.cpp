//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "Context.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "ClassdefParser.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "FunctionsInMemory.hpp"
#include "IsValidVariableName.hpp"
#include "PathFunctionIndexerManager.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    bool
    isWildcardImport(const std::string& importSpec)
    {
        return importSpec.size() > 2 && importSpec.compare(importSpec.size() - 2, 2, ".*") == 0;
    }
    //=============================================================================
    std::string
    importLeafName(const std::string& importSpec)
    {
        size_t pos = importSpec.find_last_of('.');
        if (pos == std::string::npos || pos + 1 >= importSpec.size()) {
            return importSpec;
        }
        return importSpec.substr(pos + 1);
    }
    //=============================================================================
    bool
    isValidImportSpec(const std::string& importSpec)
    {
        if (importSpec.empty()) {
            return false;
        }
        bool wildcard = isWildcardImport(importSpec);
        size_t end = wildcard ? importSpec.size() - 2 : importSpec.size();
        if (end == 0 || importSpec.find('.') == std::string::npos) {
            return false;
        }
        size_t start = 0;
        while (start < end) {
            size_t pos = importSpec.find('.', start);
            size_t stop = pos == std::string::npos || pos > end ? end : pos;
            if (stop == start || !IsValidVariableName(importSpec.substr(start, stop - start))) {
                return false;
            }
            if (pos == std::string::npos || pos >= end) {
                break;
            }
            start = pos + 1;
        }
        if (!wildcard && importSpec.find('*') != std::string::npos) {
            return false;
        }
        if (wildcard && importSpec.find('*') != importSpec.size() - 1) {
            return false;
        }
        return true;
    }
    //=============================================================================
} // namespace
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

    if (scopestack.back()->lookupFunction(funcName, val)) {
        return true;
    }

    if (resolveImportedFunction(funcName, val)) {
        return true;
    }

    return lookupFunctionWithoutImports(funcName, val);
}
//=============================================================================
bool
Context::lookupFunctionWithoutImports(const std::string& funcName, FunctionDefPtr& val)
{
    bool found = false;
    if (FunctionsInMemory::getInstance()->find(funcName, val)) {
        if (val->type() != NLS_MACRO_FUNCTION && val->type() != NLS_MEX_FUNCTION) {
            return true;
        }
        // A poll that reports a change has rehashed, and rehashing lets go of a
        // name whose file has been removed. Read the table again rather than
        // answering with a definition it may just have dropped.
        if (!PathFunctionIndexerManager::getInstance()->hashOnFileWatcher()
            || FunctionsInMemory::getInstance()->find(funcName, val)) {
            return true;
        }
    } else {
        PathFunctionIndexerManager::getInstance()->hashOnFileWatcher();
    }
    found = PathFunctionIndexerManager::getInstance()->findIndexed(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }

    if (isInMacroFile()) {
        std::wstring directory = scopestack.back()->getDirectory();
        const std::string privateFunctionName = getMangledPrivateFunction(directory, funcName);
        if (FunctionsInMemory::getInstance()->find(privateFunctionName, val)) {
            if (val->type() != NLS_MACRO_FUNCTION && val->type() != NLS_MEX_FUNCTION) {
                return true;
            }
            if (!PathFunctionIndexerManager::getInstance()->hashOnFileWatcher()
                || FunctionsInMemory::getInstance()->find(privateFunctionName, val)) {
                return true;
            }
        }
        found = PathFunctionIndexerManager::getInstance()->findIndexed(privateFunctionName, val);
        if (found) {
            FunctionsInMemory::getInstance()->add(privateFunctionName, val);
            return true;
        }
    }

    // The indexes hold nothing under that name, as a plain function or as a
    // private one. A file may have been written to a watched directory without
    // its change notification having been delivered yet, so probe the disk
    // before falling back on the built-ins, which keeps a file that has just
    // appeared ahead of a built-in of the same name. The probe costs two file
    // stats per watched directory, which is why it runs here rather than on
    // every lookup that the indexes cannot answer: calling a private function
    // misses under its plain name every single time.
    if (PathFunctionIndexerManager::getInstance()->rehashWatchedPathOwningFile(funcName)
        && PathFunctionIndexerManager::getInstance()->findIndexed(funcName, val)) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }

    found = BuiltInFunctionDefManager::getInstance()->find(funcName, val);
    if (found) {
        FunctionsInMemory::getInstance()->add(funcName, val);
        return true;
    }
    return scopestack.front()->lookupFunction(funcName, val);
}
//=============================================================================
bool
Context::resolveImportedFunction(const std::string& funcName, FunctionDefPtr& val)
{
    Scope* currentScope = getCurrentScope();
    if (currentScope == nullptr) {
        return false;
    }
    stringVector imports = currentScope->getImports();
    for (const auto& importSpec : imports) {
        if (isWildcardImport(importSpec)) {
            std::string prefix = importSpec.substr(0, importSpec.size() - 2);
            if (resolveImportCandidate(prefix + "." + funcName, val)) {
                return true;
            }
        } else if (importLeafName(importSpec) == funcName) {
            if (resolveImportCandidate(importSpec, val)) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
Context::resolveImportCandidate(const std::string& importSpec, FunctionDefPtr& val)
{
    if (lookupFunctionWithoutImports(importSpec, val)) {
        return true;
    }
    size_t pos = importSpec.find_last_of('.');
    if (pos == std::string::npos || pos == 0 || pos + 1 >= importSpec.size()) {
        return false;
    }
    std::string className = importSpec.substr(0, pos);
    std::string memberName = importSpec.substr(pos + 1);
    std::string functionName;
    if (!classdefResolveStaticMethodFunction(className, memberName, functionName)) {
        return false;
    }
    return lookupFunctionWithoutImports(functionName, val);
}
//=============================================================================
bool
Context::addImport(const std::string& importSpec)
{
    if (!isValidImportSpec(importSpec)) {
        Error(L"Nelson:interpreter:invalidImportName", L"A valid import name expected.");
    }
    FunctionDefPtr importedFunction = nullptr;
    if (!isWildcardImport(importSpec) && !resolveImportCandidate(importSpec, importedFunction)) {
        Error(L"Nelson:interpreter:importNotFound",
            L"Unable to find or import '%s'. Imported names must end with '.*' or be fully "
            L"qualified.",
            utf8_to_wstring(importSpec));
    }
    Scope* currentScope = getCurrentScope();
    return currentScope != nullptr && currentScope->addImport(importSpec);
}
//=============================================================================
void
Context::clearImports()
{
    Scope* currentScope = getCurrentScope();
    if (currentScope != nullptr) {
        currentScope->clearImports();
    }
}
//=============================================================================
stringVector
Context::getImports()
{
    Scope* currentScope = getCurrentScope();
    return currentScope == nullptr ? stringVector() : currentScope->getImports();
}
//=============================================================================
bool
Context::resolveImportedClassName(const std::string& className, std::string& resolvedClassName)
{
    if (classdefLoadClass(className)) {
        resolvedClassName = className;
        return true;
    }
    Scope* currentScope = getCurrentScope();
    if (currentScope == nullptr) {
        return false;
    }
    stringVector imports = currentScope->getImports();
    for (const auto& importSpec : imports) {
        if (isWildcardImport(importSpec)) {
            std::string candidate = importSpec.substr(0, importSpec.size() - 2) + "." + className;
            if (classdefLoadClass(candidate)) {
                resolvedClassName = candidate;
                return true;
            }
        } else if (importLeafName(importSpec) == className && classdefLoadClass(importSpec)) {
            resolvedClassName = importSpec;
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
