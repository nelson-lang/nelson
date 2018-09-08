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
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDefManager* BuiltInFunctionDefManager::m_pInstance = nullptr;
//=============================================================================
BuiltInFunctionDefManager::BuiltInFunctionDefManager() { clearCache(); }
//=============================================================================
void
BuiltInFunctionDefManager::destroy()
{
    clearCache();
    removeAll();
    if (m_pInstance) {
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
BuiltInFunctionDefManager::add(FuncPtr ptr)
{
    if (ptr) {
        builtinVector.push_back(ptr);
        return true;
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::add(std::string name, BuiltInFuncPtr fptr, int argc_in, int argc_out,
    std::wstring dynlibname, std::wstring modulename)
{
    BuiltInFunctionDef* f2def;
    try {
        f2def = new BuiltInFunctionDef();
    } catch (const std::bad_alloc&) {
        f2def = nullptr;
    }
    if (f2def) {
        stringVector args;
        f2def->hashid = std::hash<std::wstring>()(utf8_to_wstring(name) + L"_" + modulename);
        f2def->fileName = dynlibname;
        f2def->retCount = argc_out;
        f2def->argCount = argc_in;
        f2def->name = name;
        f2def->fptr = fptr;
        f2def->arguments = args;
        return add(f2def);
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(std::string name)
{
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        if ((*it)->name == name) {
            BuiltInFunctionDef* p = (BuiltInFunctionDef*)*it;
            delete p;
            builtinVector.erase(it);
            clearCache();
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(FuncPtr ptr)
{
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        if (*it == ptr) {
            BuiltInFunctionDef* p = (BuiltInFunctionDef*)(*it);
            delete p;
            clearCache();
            builtinVector.erase(it);
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(BuiltInFunctionDef* ptr)
{
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        if (*it == ptr) {
            BuiltInFunctionDef* p = (BuiltInFunctionDef*)(*it);
            delete p;
            clearCache();
            builtinVector.erase(it);
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(BuiltInFuncPtr fptr)
{
    bool res = false;
    for (size_t k = 0; k < builtinVector.size(); k++) {
        BuiltInFunctionDef* asBuiltInPtr = (BuiltInFunctionDef*)builtinVector[k];
        if (asBuiltInPtr != nullptr) {
            if (asBuiltInPtr->fptr == fptr) {
                delete asBuiltInPtr;
                builtinVector.erase(builtinVector.begin() + k);
                res = true;
            }
        }
    }
    if (res) {
        clearCache();
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::removeAll()
{
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        delete *it;
    }
    builtinVector.clear();
    clearCache();
    return false;
}
//=============================================================================
std::vector<FuncPtr>
BuiltInFunctionDefManager::getTable()
{
    std::vector<FuncPtr> builtinTable;
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        builtinTable.push_back(*it);
    }
    return builtinTable;
}
//=============================================================================
stringVector
BuiltInFunctionDefManager::getNameList()
{
    stringVector nameList;
    for (std::vector<FuncPtr>::iterator it = builtinVector.begin(); it != builtinVector.end();
         ++it) {
        nameList.push_back((*it)->name);
    }
    return nameList;
}
//=============================================================================
bool
BuiltInFunctionDefManager::isPointerOnBuiltInFunctionDef(FuncPtr ptr)
{
    if (builtinVector.size() > 0) {
        for (std::vector<FuncPtr>::reverse_iterator it = builtinVector.rbegin();
             it != builtinVector.rend(); ++it) {
            if (*it == ptr) {
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string name, std::wstring& path)
{
    bool res = false;
    if (builtinVector.size() > 0) {
        for (std::vector<FuncPtr>::reverse_iterator it = builtinVector.rbegin();
             it != builtinVector.rend(); ++it) {
            if ((*it)->name == name) {
                path = ((BuiltInFunctionDef*)(*it))->fileName;
                return true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(size_t hashid, std::wstring& functionname)
{
    bool res = false;
    if (builtinVector.size() > 0) {
        for (std::vector<FuncPtr>::reverse_iterator it = builtinVector.rbegin();
             it != builtinVector.rend(); ++it) {
            if ((*it)->hashid == hashid) {
                functionname = utf8_to_wstring((*it)->name);
                return true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string name, wstringVector& paths)
{
    bool res = false;
    if (builtinVector.size() > 0) {
        for (std::vector<FuncPtr>::reverse_iterator it = builtinVector.rbegin();
             it != builtinVector.rend(); ++it) {
            if ((*it)->name == name) {
                paths.push_back(((BuiltInFunctionDef*)(*it))->fileName);
                res = true;
            }
        }
    }
    return res;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string name, FuncPtr& ptr)
{
    if (builtinVector.size() > 0) {
        std::unordered_map<std::string, FuncPtr>::const_iterator found = cachedBuiltin.find(name);
        if (found != cachedBuiltin.end()) {
            ptr = found->second;
            return true;
        } else {
            for (std::vector<FuncPtr>::reverse_iterator it = builtinVector.rbegin();
                 it != builtinVector.rend(); ++it) {
                if ((*it)->name == name) {
                    ptr = (FuncPtr)(*it);
                    cachedBuiltin.emplace(name, ptr);
                    return true;
                }
            }
        }
    }
    return false;
}
//=============================================================================
void
BuiltInFunctionDefManager::clearCache()
{
    cachedBuiltin.clear();
}
//=============================================================================
}
//=============================================================================
