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
FunctionsInMemory::add(
    Overload::OverloadClass overloadClass, const std::string& functionName, FunctionDefPtr function)
{
    if (function != nullptr) {
        switch (overloadClass) {
        case Overload::UNARY: {
            _lastUnaryFunctionInMemory = std::make_pair(functionName, function);
        } break;
        case Overload::BINARY: {
            _lastBinaryFunctionInMemory = std::make_pair(functionName, function);
        } break;
        case Overload::TERNARY: {
            _lastTernaryFunctionInMemory = std::make_pair(functionName, function);
        } break;
        default: {
        } break;
        }
    }
}
//=============================================================================
void
FunctionsInMemory::add(const std::string& functionName, FunctionDefPtr function)
{
    if (function != nullptr) {
        if (function->type() == NLS_MACRO_FUNCTION) {
            _macroFunctionsInMemory.push_back(std::make_pair(functionName, function));
        } else if (function->type() == NLS_MEX_FUNCTION) {
            _mexfunctionsInMemory.push_back(std::make_pair(functionName, function));
        }
    }
}
//=============================================================================
void
FunctionsInMemory::clearOverloadFunctionsInMemory()
{ 
  _lastUnaryFunctionInMemory.first.clear();
  _lastUnaryFunctionInMemory.second = nullptr;
  _lastBinaryFunctionInMemory.first.clear();
  _lastBinaryFunctionInMemory.second = nullptr;
  _lastTernaryFunctionInMemory.first.clear();
  _lastTernaryFunctionInMemory.second = nullptr;
}
//=============================================================================
void
FunctionsInMemory::clearOverloadFunctionInMemory(const std::string& functionName)
{ 
    if (_lastUnaryFunctionInMemory.first == functionName) {
        _lastUnaryFunctionInMemory.first.clear();
        _lastUnaryFunctionInMemory.second = nullptr;
    }
    if (_lastBinaryFunctionInMemory.first == functionName) {
        _lastBinaryFunctionInMemory.first.clear();
        _lastBinaryFunctionInMemory.second = nullptr;
    }
    if (_lastTernaryFunctionInMemory.first == functionName) {
        _lastTernaryFunctionInMemory.first.clear();
        _lastTernaryFunctionInMemory.second = nullptr;
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
                return true;
            }
        }
    }
    clearOverloadFunctionInMemory(functionName);
    return false;
}
//=============================================================================
bool
FunctionsInMemory::deleteMexFunction(const std::string& functionName)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _mexfunctionsInMemory.rbegin();
         it != _mexfunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            MexFunctionDef* f = (MexFunctionDef*)it->second;
            if (f != nullptr) {
                if (!f->isLocked()) {
                    delete f;
                    f = nullptr;
                    _mexfunctionsInMemory.erase((it + 1).base());
                    return true;
                }
            }
        }
    }
    clearOverloadFunctionInMemory(functionName);
    return false;
}
//=============================================================================
bool
FunctionsInMemory::find(const std::string& functionName, FunctionDefPtr& function)
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator it
         = _mexfunctionsInMemory.rbegin();
         it != _mexfunctionsInMemory.rend(); ++it) {
        if (it->first == functionName) {
            function = it->second;
            return true;
        }
    }

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
FunctionsInMemory::find(Overload::OverloadClass overloadClass, const std::string& functionName,
    FunctionDefPtr& function)
{
    switch (overloadClass) {
    case Overload::UNARY: {
        if (functionName == _lastUnaryFunctionInMemory.first) {
            function = _lastUnaryFunctionInMemory.second;
            return true;
        }
    } break;
    case Overload::BINARY: {
        if (functionName == _lastBinaryFunctionInMemory.first) {
            function = _lastBinaryFunctionInMemory.second;
            return true;
        }
    } break;
    case Overload::TERNARY: {
        if (functionName == _lastTernaryFunctionInMemory.first) {
            function = _lastTernaryFunctionInMemory.second;
            return true;
        }
    } break;
    default: {
    } break;
    }
    function = nullptr;
    return false;
}
//=============================================================================
void
FunctionsInMemory::deleteAllMFunctions()
{
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::iterator iter
         = _macroFunctionsInMemory.begin();
         iter != _macroFunctionsInMemory.end(); ++iter) {
        FunctionDefPtr funPtr = iter->second;
        MacroFunctionDef* f = (MacroFunctionDef*)funPtr;
        if (f != nullptr) {
            delete f;
            f = nullptr;
        }
    }
    _macroFunctionsInMemory.clear();
    clearOverloadFunctionsInMemory();
}
//=============================================================================
bool
FunctionsInMemory::deleteAllMexFunctions()
{
    bool noLocked = true;
    std::vector<std::pair<std::string, FunctionDefPtr>> lockedMex;
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::iterator iter
         = _mexfunctionsInMemory.begin();
         iter != _mexfunctionsInMemory.end(); ++iter) {
        FunctionDefPtr funPtr = iter->second;
        MexFunctionDef* f = (MexFunctionDef*)funPtr;
        if (f != nullptr) {
            if (!f->isLocked()) {
                delete f;
                f = nullptr;
            } else {
                lockedMex.push_back(std::make_pair(iter->first, iter->second));
                noLocked = false;
            }
        }
    }
    _mexfunctionsInMemory.clear();
    _mexfunctionsInMemory = lockedMex;
    clearOverloadFunctionsInMemory();
    return noLocked;
}
//=============================================================================
void
FunctionsInMemory::clear(stringVector exceptedFunctions)
{
    if (exceptedFunctions.empty()) {
        for (auto it = _mexfunctionsInMemory.rbegin(); it != _mexfunctionsInMemory.rend();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            if (funcPtr->isLocked()) {
                it++;
            } else {
                delete funcPtr;
                funcPtr = nullptr;
                it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                    _mexfunctionsInMemory.erase((++it).base()));
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

        for (auto it = _mexfunctionsInMemory.rbegin(); it != _mexfunctionsInMemory.rend();) {
            MexFunctionDef* funcPtr = (MexFunctionDef*)it->second;
            stringVector::iterator iter
                = std::find(exceptedFunctions.begin(), exceptedFunctions.end(), funcPtr->getName());
            if (iter == exceptedFunctions.end()) {
                if (funcPtr->isLocked()) {
                    it++;
                } else {
                    delete funcPtr;
                    funcPtr = nullptr;
                    it = std::vector<std::pair<std::string, FunctionDefPtr>>::reverse_iterator(
                        _mexfunctionsInMemory.erase((++it).base()));
                }
            } else {
                it++;
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
                it++;
            }
        }
    }
    clearOverloadFunctionsInMemory();
}
//=============================================================================
wstringVector
FunctionsInMemory::getMacroInMemory(bool withCompleteNames)
{
    wstringVector names;
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::iterator it
         = _macroFunctionsInMemory.begin();
         it != _macroFunctionsInMemory.end(); ++it) {
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
    for (std::vector<std::pair<std::string, FunctionDefPtr>>::iterator it
         = _mexfunctionsInMemory.begin();
         it != _mexfunctionsInMemory.end(); ++it) {
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
