//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "FunctionsInMemory.hpp"
#include "MexFunctionDef.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
FunctionsInMemory* FunctionsInMemory::m_pInstance = nullptr;
//=============================================================================
FunctionsInMemory::FunctionsInMemory() = default;
//=============================================================================
FunctionsInMemory::~FunctionsInMemory()
{
    clearMapCache();
    _macroFunctionsInMemory.clear();
    _mexFunctionsInMemory.clear();
    _builtinFunctionInMemory.clear();
}
//=============================================================================
FunctionsInMemory*
FunctionsInMemory::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new FunctionsInMemory();
    }
    return m_pInstance;
}
//=============================================================================
void
FunctionsInMemory::destroy()
{
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
void
FunctionsInMemory::clearMapCache()
{
    _lastFunctionsInMemory.clear();
}
//=============================================================================
void
FunctionsInMemory::add(const std::string& functionName, FunctionDefPtr function)
{
    if (function != nullptr) {
        if (function->type() == NLS_MACRO_FUNCTION) {
            _macroFunctionsInMemory.emplace_back(std::make_pair(functionName, function));
        } else if (function->type() == NLS_MEX_FUNCTION) {
            _mexFunctionsInMemory.emplace_back(std::make_pair(functionName, function));
        } else if (function->type() == NLS_BUILT_IN_FUNCTION) {
            _builtinFunctionInMemory.emplace(functionName, function);
        }
    }
}
//=============================================================================
bool
FunctionsInMemory::deleteMFunction(const std::string& functionName)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _macroFunctionsInMemory.rbegin();
         it != _macroFunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            MexFunctionDef* f = (MexFunctionDef*)it->second;
            if (f != nullptr) {
                delete f;
                f = nullptr;
                _macroFunctionsInMemory.erase((it + 1).base());
                clearMapCache();
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::deleteMexFunction(const std::string& functionName)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _mexFunctionsInMemory.rbegin();
         it != _mexFunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            MexFunctionDef* f = (MexFunctionDef*)it->second;
            if (f != nullptr) {
                if (!f->isLocked()) {
                    delete f;
                    f = nullptr;
                    clearMapCache();
                    _mexFunctionsInMemory.erase((it + 1).base());
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::findMex(const std::string& functionName, FunctionDefPtr& function)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _mexFunctionsInMemory.rbegin();
         it != _mexFunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            function = it->second;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::findMacro(const std::string& functionName, FunctionDefPtr& function)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _macroFunctionsInMemory.rbegin();
         it != _macroFunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            function = it->second;
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::findBuiltin(const std::string& functionName, FunctionDefPtr& function)
{
    auto it = _builtinFunctionInMemory.find(functionName);
    if (it != _builtinFunctionInMemory.end()) {
        function = it->second;
        return true;
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::find(
    const std::string& functionName, FunctionDefPtr& function, FIND_FUNCTION_TYPE functionType)
{
    switch (functionType) {
    case FIND_FUNCTION_TYPE::ALL: {

        auto it = _lastFunctionsInMemory.find(functionName);
        if (it != _lastFunctionsInMemory.end()) {
            function = it->second;
            return true;
        }

        if (findMex(functionName, function)) {
            _lastFunctionsInMemory.emplace(functionName, function);
            return true;
        }
        if (findMacro(functionName, function)) {
            _lastFunctionsInMemory.emplace(functionName, function);
            return true;
        }
        if (findBuiltin(functionName, function)) {
            _lastFunctionsInMemory.emplace(functionName, function);
            return true;
        }
        return false;
    } break;
    case FIND_FUNCTION_TYPE::MACRO: {
        return findMacro(functionName, function);
    } break;
    case FIND_FUNCTION_TYPE::MEX: {
        return findMex(functionName, function);
    } break;
    case FIND_FUNCTION_TYPE::BUILTIN: {
        return findBuiltin(functionName, function);
    }
    default: { } break; }
    return false;
}
//=============================================================================
void
FunctionsInMemory::deleteAllMFunctions()
{
    for (auto& iter : _macroFunctionsInMemory) {
        FunctionDefPtr funPtr = iter.second;
        MacroFunctionDef* f = (MacroFunctionDef*)funPtr;
        if (f != nullptr) {
            delete f;
            f = nullptr;
        }
    }
    _macroFunctionsInMemory.clear();
    clearMapCache();
}
//=============================================================================
bool
FunctionsInMemory::deleteAllMexFunctions()
{
    bool noLocked = true;
    std::vector<std::pair<std::string, FunctionDefPtr>> lockedMex;
    for (auto& iter : _mexFunctionsInMemory) {
        FunctionDefPtr funPtr = iter.second;
        MexFunctionDef* f = (MexFunctionDef*)funPtr;
        if (f != nullptr) {
            if (!f->isLocked()) {
                delete f;
                f = nullptr;
            } else {
                lockedMex.emplace_back(std::make_pair(iter.first, iter.second));
                noLocked = false;
            }
        }
    }
    _mexFunctionsInMemory.clear();
    _builtinFunctionInMemory.clear();
    clearMapCache();
    return noLocked;
}
//=============================================================================
void
FunctionsInMemory::clear(stringVector exceptedFunctions)
{
    if (exceptedFunctions.empty()) {
        for (auto it = _mexFunctionsInMemory.rbegin(); it != _mexFunctionsInMemory.rend();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            if (funcPtr->isLocked()) {
                ++it;
            } else {
                delete funcPtr;
                funcPtr = nullptr;
                it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                    _mexFunctionsInMemory.erase((++it).base()));
            }
        }

        for (auto it = _macroFunctionsInMemory.rbegin(); it != _macroFunctionsInMemory.rend();) {
            MacroFunctionDef* funcPtr = (MacroFunctionDef*)it->second;
            delete funcPtr;
            funcPtr = nullptr;
            it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                _macroFunctionsInMemory.erase((it + 1).base()));
        }
    } else {
        for (auto it = _mexFunctionsInMemory.rbegin(); it != _mexFunctionsInMemory.rend();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            stringVector::iterator iter
                = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), funcPtr->getName());
            if (iter == exceptedFunctions.end()) {
                if (funcPtr->isLocked()) {
                    ++it;
                } else {
                    delete funcPtr;
                    funcPtr = nullptr;
                    it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                        _mexFunctionsInMemory.erase((++it).base()));
                }
            } else {
                ++it;
            }
        }

        for (auto it = _macroFunctionsInMemory.rbegin(); it != _macroFunctionsInMemory.rend();) {
            MacroFunctionDef* funcPtr = (MacroFunctionDef*)it->second;
            stringVector::iterator iter
                = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), funcPtr->getName());
            if (iter == exceptedFunctions.end()) {
                delete funcPtr;
                funcPtr = nullptr;
                it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                    _macroFunctionsInMemory.erase((++it).base()));
            } else {
                ++it;
            }
        }
    }
    _builtinFunctionInMemory.clear();
    clearMapCache();
}
//=============================================================================
wstringVector
FunctionsInMemory::getMacroInMemory(bool withCompleteNames)
{
    wstringVector names;
    for (auto& it : _macroFunctionsInMemory) {
        MacroFunctionDef* fptr = (MacroFunctionDef*)it.second;
        if (withCompleteNames) {
            names.emplace_back(fptr->getFilename());
        } else {
            names.emplace_back(utf8_to_wstring(fptr->getName()));
        }
    }
    return names;
}
//=============================================================================
wstringVector
FunctionsInMemory::getMexInMemory(bool withCompleteNames)
{
    wstringVector names;
    for (auto& it : _mexFunctionsInMemory) {
        MexFunctionDef* fptr = (MexFunctionDef*)it.second;
        if (withCompleteNames) {
            names.emplace_back(fptr->getFilename());
        } else {
            names.emplace_back(utf8_to_wstring(fptr->getName()));
        }
    }
    return names;
}
//=============================================================================
}
//=============================================================================
