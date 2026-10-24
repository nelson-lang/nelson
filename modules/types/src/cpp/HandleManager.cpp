//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HandleManager.hpp"
//=============================================================================
namespace Nelson {
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
HandleManager::HandleManager() = default;
//=============================================================================
void
HandleManager::destroy()
{
    HandleRegistry::instance().clear();
}
//=============================================================================
HandleRef
HandleManager::addHandleRef(HandleGenericObject* ptr)
{
    return HandleRegistry::instance().add(ptr);
}
//=============================================================================
HandleRef
HandleManager::addInvalidHandleRef(const std::string& categoryName)
{
    return HandleRegistry::instance().addInvalid(categoryName);
}
//=============================================================================
HandleRef
HandleManager::adoptHandleRef(nelson_handle hl, HandleGenericObject* ptr)
{
    return HandleRegistry::instance().adopt(hl, ptr);
}
//=============================================================================
nelson_handle
HandleManager::addHandle(HandleGenericObject* ptr)
{
    HandleRef ref = addHandleRef(ptr);
    return ref ? ref->id() : static_cast<nelson_handle>(0);
}
//=============================================================================
HandleRef
HandleManager::getHandleRef(nelson_handle hl)
{
    return HandleRegistry::instance().find(hl);
}
//=============================================================================
nelson_handle
HandleManager::findByPointerValue(void* ptr)
{
    HandleRef ref = HandleRegistry::instance().findByPointerValue(ptr);
    return ref ? ref->id() : static_cast<nelson_handle>(-1);
}
//=============================================================================
bool
HandleManager::removeHandle(nelson_handle hl)
{
    HandleRef ref = HandleRegistry::instance().find(hl);
    if (ref) {
        ref->invalidate(nullptr);
    }
    return HandleRegistry::instance().remove(hl);
}
//=============================================================================
HandleGenericObject*
HandleManager::pointer(nelson_handle hl)
{
    HandleRef ref = HandleRegistry::instance().find(hl);
    return ref ? ref->object() : nullptr;
}
//=============================================================================
bool
HandleManager::isValid(nelson_handle hl)
{
    HandleRef ref = HandleRegistry::instance().find(hl);
    return ref && ref->isValid();
}
//=============================================================================
std::vector<nelson_handle>
HandleManager::getAllHandlesOfCategory(const std::string& category)
{
    std::vector<HandleRef> refs = getAllHandleRefsOfCategory(category);
    std::vector<nelson_handle> result;
    result.reserve(refs.size());
    for (const HandleRef& ref : refs) {
        result.push_back(ref->id());
    }
    return result;
}
//=============================================================================
std::vector<HandleRef>
HandleManager::getAllHandleRefsOfCategory(const std::string& category)
{
    return HandleRegistry::instance().getAllHandlesOfCategory(category);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
