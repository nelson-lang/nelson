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
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDefManager* BuiltInFunctionDefManager::m_pInstance = nullptr;
//=============================================================================
BuiltInFunctionDefManager::BuiltInFunctionDefManager() { }
//=============================================================================
void
BuiltInFunctionDefManager::destroy()
{
    removeAll();
    if (m_pInstance != nullptr) {
        delete m_pInstance;
        m_pInstance = nullptr;
    }
}
//=============================================================================
BuiltInFunctionDefManager*
BuiltInFunctionDefManager::getInstance()
{
    if (m_pInstance == nullptr) {
        try {
            m_pInstance = new BuiltInFunctionDefManager();
        } catch (const std::bad_alloc&) {
            m_pInstance = nullptr;
        }
    }
    return m_pInstance;
}
//=============================================================================
bool
BuiltInFunctionDefManager::add(FunctionDefPtr ptr)
{
    if (ptr != nullptr) {
        builtinVector.push_back(ptr);
        return true;
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::add(const std::string& name, void* fptr, int argc_in, int argc_out,
    const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
    bool interleavedComplex)
{
    BuiltInFunctionDef* f2def;
    try {
        f2def = new BuiltInFunctionDef();
    } catch (const std::bad_alloc&) {
        f2def = nullptr;
    }
    if (f2def != nullptr) {
        stringVector args;
        f2def->setFilename(dynlibname);
        f2def->retCount = argc_out;
        f2def->argCount = argc_in;
        f2def->setName(name);
        f2def->fptr = fptr;
        f2def->interleavedComplex = interleavedComplex;
        f2def->arguments = std::move(args);
        f2def->builtinPrototype = builtinPrototype;
        return add(f2def);
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(const std::string& name)
{
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if ((*it)->getName() == name) {
            auto* p = (BuiltInFunctionDef*)*it;
            delete p;
            builtinVector.erase(it);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(FunctionDefPtr ptr)
{
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            builtinVector.erase(it);
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(BuiltInFunctionDef* ptr)
{
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            builtinVector.erase(it);
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(void* fptr)
{
    bool res = false;
    for (size_t k = 0; k < builtinVector.size(); k++) {
        auto* asBuiltInPtr = (BuiltInFunctionDef*)builtinVector[k];
        if (asBuiltInPtr != nullptr) {
            if (asBuiltInPtr->fptr == fptr) {
                delete asBuiltInPtr;
                builtinVector.erase(builtinVector.begin() + k);
                res = true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::removeAll()
{
    for (auto& it : builtinVector) {
        delete it;
    }
    builtinVector.clear();
    return false;
}
//=============================================================================
std::vector<FunctionDefPtr>
BuiltInFunctionDefManager::getTable()
{
    std::vector<FunctionDefPtr> builtinTable;
    builtinTable.reserve(builtinVector.size());
    for (auto& it : builtinVector) {
        builtinTable.push_back(it);
    }
    return builtinTable;
}
//=============================================================================
stringVector
BuiltInFunctionDefManager::getNameList()
{
    stringVector nameList;
    nameList.reserve(builtinVector.size());
    for (auto& it : builtinVector) {
        nameList.push_back(it->getName());
    }
    return nameList;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string& name, std::wstring& path)
{
    bool res = false;
    if (!builtinVector.empty()) {
        for (auto it = builtinVector.rbegin(); it != builtinVector.rend(); ++it) {
            if ((*it)->getName() == name) {
                path = ((BuiltInFunctionDef*)(*it))->getFilename();
                return true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string& name, wstringVector& paths)
{
    bool res = false;
    if (!builtinVector.empty()) {
        for (auto it = builtinVector.rbegin(); it != builtinVector.rend(); ++it) {
            if ((*it)->getName() == name) {
                paths.push_back(((BuiltInFunctionDef*)(*it))->getFilename());
                res = true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string& name, FunctionDefPtr& ptr)
{
    if (!builtinVector.empty()) {
        for (auto it = builtinVector.rbegin(); it != builtinVector.rend(); ++it) {
            if ((*it)->getName() == name) {
                ptr = static_cast<FunctionDefPtr>(*it);
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
