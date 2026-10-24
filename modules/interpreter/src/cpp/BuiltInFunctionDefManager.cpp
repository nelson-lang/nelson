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
namespace {
    //=============================================================================
    bool
    isCallerContextNeutralBuiltin(const std::string& name)
    {
        // classdef helper builtins take an Evaluator only to run class member
        // functions (constructors, accessors, validators); they never read or
        // write the calling workspace by name, so call sites can skip the
        // locals-to-context sync that evaluator builtins otherwise force
        if (name.rfind("__classdef_", 0) == 0 || name == "__containers_map__") {
            return true;
        }
        // sprintf takes an Evaluator only to resolve overloaded conversions of
        // its own arguments; it never touches the calling workspace by name.
        // Marking it neutral removes the per-call locals-to-context sync, which
        // otherwise co-owns every caller local and makes building a struct with
        // sprintf-generated field names in a loop O(nFields^2).
        return name == "sprintf";
    }
    //=============================================================================
} // namespace
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
        bumpGeneration();
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
    for (auto* funcDef : builtinVector) {
        auto* builtinDef = (BuiltInFunctionDef*)funcDef;
        if (builtinDef != nullptr && builtinDef->isLazyGateway() && builtinDef->getName() == name
            && builtinDef->getFilename() == dynlibname) {
            builtinDef->retCount = argc_out;
            builtinDef->argCount = argc_in;
            builtinDef->fptr = fptr;
            builtinDef->interleavedComplex = interleavedComplex;
            builtinDef->builtinPrototype = builtinPrototype;
            builtinDef->overloadAutoMode = builtinOverloadAutoMode;
            builtinDef->moduleName = modulename;
            builtinDef->lazyGateway.store(false, std::memory_order_release);
            builtinHashMap[name] = builtinDef;
            return true;
        }
    }
    BuiltInFunctionDef* f2def;
    try {
        f2def = new BuiltInFunctionDef(name[0] == OVERLOAD_SYMBOL_CHAR);
    } catch (const std::bad_alloc&) {
        f2def = nullptr;
    }
    if (f2def != nullptr) {
        stringVector args;
        f2def->setFilename(dynlibname);
        f2def->moduleName = modulename;
        f2def->retCount = argc_out;
        f2def->argCount = argc_in;
        f2def->setName(name);
        f2def->fptr = fptr;
        f2def->interleavedComplex = interleavedComplex;
        f2def->lazyGateway = false;
        f2def->arguments = std::move(args);
        f2def->builtinPrototype = builtinPrototype;
        f2def->overloadAutoMode = builtinOverloadAutoMode;
        f2def->callerContextNeutral = isCallerContextNeutralBuiltin(name);
        return add(f2def);
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::addLazyGateway(const std::string& name, int argc_in, int argc_out,
    const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
    FunctionOverloadAutoMode builtinOverloadAutoMode)
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
        f2def->moduleName = modulename;
        f2def->retCount = argc_out;
        f2def->argCount = argc_in;
        f2def->setName(name);
        f2def->fptr = nullptr;
        f2def->interleavedComplex = true;
        f2def->lazyGateway = true;
        f2def->arguments = std::move(args);
        f2def->builtinPrototype = builtinPrototype;
        f2def->overloadAutoMode = builtinOverloadAutoMode;
        f2def->callerContextNeutral = isCallerContextNeutralBuiltin(name);
        return add(f2def);
    }
    return false;
}
//=============================================================================
bool
BuiltInFunctionDefManager::setCallerContextAccess(ptrBuiltin fptr, bool callerContextAccess)
{
    bool found = false;
    for (auto* funcDef : builtinVector) {
        auto* builtinDef = (BuiltInFunctionDef*)funcDef;
        if (builtinDef != nullptr && builtinDef->fptr == fptr) {
            builtinDef->callerContextAccess = callerContextAccess;
            found = true;
        }
    }
    return found;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(const std::string& name)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end();) {
        if ((*it)->getName() == name) {
            auto* p = (BuiltInFunctionDef*)*it;
            delete p;
            it = builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
            ++it;
        }
    }
    if (deleted) {
        bumpGeneration();
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::removeByFilename(const std::wstring& dynlibname)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end();) {
        if ((*it)->getFilename() == dynlibname) {
            auto* p = (BuiltInFunctionDef*)*it;
            delete p;
            it = builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
            ++it;
        }
    }
    if (deleted) {
        bumpGeneration();
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(FunctionDefPtr ptr)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end();) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            it = builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
            ++it;
        }
    }
    if (deleted) {
        bumpGeneration();
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(BuiltInFunctionDef* ptr)
{
    bool deleted = false;
    builtinHashMap.clear();
    for (auto it = builtinVector.begin(); it != builtinVector.end();) {
        if (*it == ptr) {
            auto* p = (BuiltInFunctionDef*)(*it);
            delete p;
            it = builtinVector.erase(it);
            deleted = true;
        } else {
            builtinHashMap[(*it)->getName()] = *it;
            ++it;
        }
    }
    if (deleted) {
        bumpGeneration();
    }
    return deleted;
}
//=============================================================================
bool
BuiltInFunctionDefManager::remove(ptrBuiltin fptr)
{
    bool res = false;
    builtinHashMap.clear();
    for (size_t k = 0; k < builtinVector.size();) {
        auto* asBuiltInPtr = (BuiltInFunctionDef*)builtinVector[k];
        if (asBuiltInPtr != nullptr && asBuiltInPtr->fptr == fptr) {
            delete asBuiltInPtr;
            builtinVector.erase(builtinVector.begin() + k);
            res = true;
        } else {
            if (asBuiltInPtr != nullptr) {
                builtinHashMap[builtinVector[k]->getName()] = builtinVector[k];
            }
            k++;
        }
    }
    if (res) {
        bumpGeneration();
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
    bumpGeneration();
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
