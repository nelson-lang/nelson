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
FunctionsInMemory::FunctionsInMemory() { }
//=============================================================================
FunctionsInMemory::~FunctionsInMemory() { }
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
FunctionsInMemory::add(const std::string& functionName, FuncPtr function)
{
    if (function != nullptr) {
        if (function->type() == NLS_MACRO_FUNCTION) {
            _MacroFunctionsInMemory.emplace(functionName, function);
        } else if (function->type() == NLS_MEX_FUNCTION) {
            _MexfunctionsInMemory.emplace(functionName, function);
        }
    }
}
//=============================================================================
bool
FunctionsInMemory::deleteMFunction(const std::string& functionName)
{
    auto it = _MacroFunctionsInMemory.find(functionName);
    if (it != _MacroFunctionsInMemory.end()) {
        MacroFunctionDef* f = (MacroFunctionDef*)it->second;
        if (f != nullptr) {
            delete f;
            f = nullptr;
            _MacroFunctionsInMemory.erase(it);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::deleteMexFunction(const std::string& functionName)
{
    auto it = _MexfunctionsInMemory.find(functionName);
    if (it != _MexfunctionsInMemory.end()) {
        MexFunctionDef* f = (MexFunctionDef*)it->second;
        if (f != nullptr) {
            if (!f->isLocked()) {
                delete f;
                f = nullptr;
                _MexfunctionsInMemory.erase(it);
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
FunctionsInMemory::find(const std::string& functionName, FuncPtr& function)
{
    auto it = _MexfunctionsInMemory.find(functionName);
    if (it != _MexfunctionsInMemory.end()) {
        function = it->second;
        return true;
    }
    it = _MacroFunctionsInMemory.find(functionName);
    if (it != _MacroFunctionsInMemory.end()) {
        function = it->second;
        return true;
    }
    return false;
}
//=============================================================================
void
FunctionsInMemory::deleteAllMFunctions()
{
    for (boost::unordered_map<std::string, FuncPtr>::iterator iter
         = _MacroFunctionsInMemory.begin();
         iter != _MacroFunctionsInMemory.end(); ++iter) {
        FuncPtr funPtr = iter->second;
        MacroFunctionDef* f = (MacroFunctionDef*)funPtr;
        if (f != nullptr) {
            delete f;
            f = nullptr;
        }
    }
    _MacroFunctionsInMemory.clear();
}
//=============================================================================
bool
FunctionsInMemory::deleteAllMexFunctions()
{
    bool noLocked = true;
    boost::unordered_map<std::string, FuncPtr> lockedMex;
    for (boost::unordered_map<std::string, FuncPtr>::iterator iter = _MexfunctionsInMemory.begin();
         iter != _MexfunctionsInMemory.end(); ++iter) {
        FuncPtr funPtr = iter->second;
        MexFunctionDef* f = (MexFunctionDef*)funPtr;
        if (f != nullptr) {
            if (!f->isLocked()) {
                delete f;
                f = nullptr;
            } else {
                lockedMex.emplace(iter->first, iter->second);
                noLocked = false;
            }
        }
    }
    _MexfunctionsInMemory.clear();
    _MexfunctionsInMemory = lockedMex;
    return noLocked;
}
//=============================================================================
void
FunctionsInMemory::clear(stringVector exceptedFunctions)
{
    if (exceptedFunctions.empty()) {
        for (auto it = _MexfunctionsInMemory.begin(); it != _MexfunctionsInMemory.end();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            if (funcPtr->isLocked()) {
                it++;
            } else {
                delete funcPtr;
                funcPtr = nullptr;
                it = _MexfunctionsInMemory.erase(it);
            }
        }

        for (auto it = _MacroFunctionsInMemory.begin(); it != _MacroFunctionsInMemory.end();) {
            MacroFunctionDef* funcPtr = (MacroFunctionDef*)it->second;
            delete funcPtr;
            funcPtr = nullptr;
            it = _MacroFunctionsInMemory.erase(it);
        }
    } else {
        for (auto it = _MexfunctionsInMemory.begin(); it != _MexfunctionsInMemory.end();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            stringVector::iterator iter
                = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), funcPtr->getName());
            if (iter == exceptedFunctions.end()) {
                if (funcPtr->isLocked()) {
                    it++;
                } else {
                    delete funcPtr;
                    funcPtr = nullptr;
                    it = _MexfunctionsInMemory.erase(it);
                }
            } else {
                it++;
            }
        }

        for (auto it = _MacroFunctionsInMemory.begin(); it != _MacroFunctionsInMemory.end();) {
            MacroFunctionDef* funcPtr = (MacroFunctionDef*)it->second;
            stringVector::iterator iter
                = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), funcPtr->getName());
            if (iter == exceptedFunctions.end()) {
                delete funcPtr;
                funcPtr = nullptr;
                it = _MacroFunctionsInMemory.erase(it);
            } else {
                it++;
            }
        }
    }
}
//=============================================================================
wstringVector
FunctionsInMemory::getMacroInMemory(bool withCompleteNames)
{
    wstringVector names;
    for (auto it = _MacroFunctionsInMemory.begin(); it != _MacroFunctionsInMemory.end(); ++it) {
        MacroFunctionDef* fptr = (MacroFunctionDef*)it->second;
        if (withCompleteNames) {
            names.push_back(fptr->getFilename());
        } else {
            names.push_back(utf8_to_wstring(fptr->getName()));
        }
    }
    return names;
}
//=============================================================================
wstringVector
FunctionsInMemory::getMexInMemory(bool withCompleteNames)
{
    wstringVector names;
    for (auto it = _MexfunctionsInMemory.begin(); it != _MexfunctionsInMemory.end(); ++it) {
        MexFunctionDef* fptr = (MexFunctionDef*)it->second;
        if (withCompleteNames) {
            names.push_back(fptr->getFilename());
        } else {
            names.push_back(utf8_to_wstring(fptr->getName()));
        }
    }
    return names;
}
//=============================================================================
}
//=============================================================================
