//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BuiltInFunctionDefManager.hpp"
#include "characters_encoding.hpp"
#include "OverloadName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
BuiltInFunctionDefManager* BuiltInFunctionDefManager::m_pInstance = nullptr;
//=============================================================================
BuiltInFunctionDefManager::BuiltInFunctionDefManager() = default;
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
        builtinHashMap[ptr->getName()] = ptr;
        return true;
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::add(const std::string& name, void* fptr, int argc_in, int argc_out,
    const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
    bool interleavedComplex, FunctionOverloadAutoMode builtinOverloadAutoMode)
{
    BuiltInFunctionDef* f2def;
    try {
        f2def = new BuiltInFunctionDef(name[0] == OVERLOAD_SYMBOL_CHAR);
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
        f2def->overloadAutoMode = builtinOverloadAutoMode;
        return add(f2def);
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(const std::string& name)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if ((*it)->getName() == name) {
            auto* p = (BuiltInFunctionDef*)*it;
            delete p;
            builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
        }
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(FunctionDefPtr ptr)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
        }
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(BuiltInFunctionDef* ptr)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end(); ++it) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
        }
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(ptrBuiltin fptr)
{
    bool res = false;
    builtinHashMap.clear();
    for (size_t k = 0; k < builtinVector.size(); k++) {
        auto* asBuiltInPtr = (BuiltInFunctionDef*)builtinVector[k];
        if (asBuiltInPtr != nullptr) {
            if (asBuiltInPtr->fptr == fptr) {
                delete asBuiltInPtr;
                builtinVector.erase(builtinVector.begin() + k);
                res = true;
            } else {
                builtinHashMap[builtinVector[k]->getName()] = builtinVector[k];
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
    builtinHashMap.clear();
    return true;
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
        nameList.emplace_back(it->getName());
    }
    return nameList;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string& name, std::wstring& path)
{
    auto it = builtinHashMap.find(name);
    if (it != builtinHashMap.end()) {
        path = it->second->getFilename();
        return true;
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::find(const std::string& name, wstringVector& paths)
{
    bool res = false;
    paths.clear();
    if (!builtinVector.empty()) {
        for (auto it = builtinVector.rbegin(); it != builtinVector.rend(); ++it) {
            if ((*it)->getName() == name) {
                paths.emplace_back(((BuiltInFunctionDef*)(*it))->getFilename());
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
    auto it = builtinHashMap.find(name);
    if (it != builtinHashMap.end()) {
        ptr = it->second;
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
