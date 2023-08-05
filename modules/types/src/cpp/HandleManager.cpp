//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static nelson_handle hash_gen = 1;
//=============================================================================
HandleManager* HandleManager::m_pInstance = nullptr;
//=============================================================================
HandleManager*
HandleManager::getInstance()
{
    if (m_pInstance == nullptr) {
        m_pInstance = new HandleManager();
    }
    return m_pInstance;
}
//=============================================================================
HandleManager::HandleManager()
{
    hash_gen = 1;
    handleMap.clear();
}
//=============================================================================
void
HandleManager::destroy()
{
    handleMap.clear();
}
//=============================================================================
nelson_handle
HandleManager::addHandle(HandleGenericObject* ptr)
{
    if (ptr == nullptr) {
        return static_cast<nelson_handle>(0);
    }
    std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.begin();
    while (it != handleMap.end()) {
        if (it->second->getPointer() == ptr->getPointer()) {
            return it->first;
        }
        ++it;
    }
    nelson_handle id = hash_gen + 1;
    handleMap.emplace(id, ptr);
    hash_gen = hash_gen + 1;
    return id;
}
//=============================================================================
nelson_handle
HandleManager::findByPointerValue(void* ptr)
{
    if (ptr != nullptr) {
        std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.begin();
        while (it != handleMap.end()) {
            if (it->second != nullptr) {
                if (it->second->getPointer() == ptr) {
                    return it->first;
                }
            }
            ++it;
        }
    }
    return -1;
}
//=============================================================================
bool
HandleManager::removeHandle(nelson_handle hl)
{
    std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.find(hl);
    if (it != handleMap.end()) {
        if (it->second != nullptr) {
            it->second = nullptr;
        }
        handleMap.erase(it);
        return true;
    }
    return false;
}
//=============================================================================
HandleGenericObject*
HandleManager::getPointer(nelson_handle hl)
{
    std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.find(hl);
    if (it != handleMap.end()) {
        return it->second;
    }
    return nullptr;
}
//=============================================================================
bool
HandleManager::isValid(nelson_handle hl)
{
    std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.find(hl);
    if (it != handleMap.end()) {
        return true;
    }
    return false;
}
//=============================================================================
std::vector<nelson_handle>
HandleManager::getAllHandlesOfCategory(const std::string& category)
{
    std::vector<nelson_handle> res;
    std::unordered_map<nelson_handle, HandleGenericObject*>::iterator it = handleMap.begin();
    while (it != handleMap.end()) {
        if (it->second != nullptr) {
            if (category == it->second->getCategory()) {
                res.push_back(it->first);
            }
        }
        ++it;
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
