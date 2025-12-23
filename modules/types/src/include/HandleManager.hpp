//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <unordered_map>
#include <string>
#include <vector>
#include "HandleGenericObject.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSTYPES_IMPEXP HandleManager
{
    //=============================================================================
public:
    static HandleManager*
    getInstance();
    void
    destroy();
    nelson_handle
    addHandle(HandleGenericObject* ptr);
    bool
    removeHandle(nelson_handle hl);
    HandleGenericObject*
    getPointer(nelson_handle hl);
    bool
    isValid(nelson_handle hl);
    std::vector<nelson_handle>
    getAllHandlesOfCategory(const std::string& category);
    nelson_handle
    findByPointerValue(void* ptr);
    //=============================================================================
private:
    HandleManager();
    std::unordered_map<nelson_handle, HandleGenericObject*> handleMap;
    static HandleManager* m_pInstance;
    //=============================================================================
};
//=============================================================================
template <typename T, typename Deleter = void (*)(T*)>
bool
DeleteHandleObjects(
    const ArrayOf& A, const std::string& expectedCategory, const std::wstring& handleExpectedMsg,
    const std::wstring& validHandleExpectedMsg, Deleter deleter = [](T* obj) { delete obj; })
{
    if (!A.isHandle()) {
        return false;
    }
    if (A.isEmpty()) {
        Error(validHandleExpectedMsg);
        return false;
    }

    Dimensions dims = A.getDimensions();
    nelson_handle* qp = (nelson_handle*)(A.getDataPointer());
    size_t elementCount = static_cast<size_t>(dims.getElementCount());
    bool res = false;

    for (size_t k = 0; k < elementCount; ++k) {
        nelson_handle hl = qp[k];
        HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
        if (!hlObj) {
            continue;
        }
        if (hlObj->getCategory() != expectedCategory) {
            Error(handleExpectedMsg);
            continue;
        }
        auto* obj = dynamic_cast<T*>(hlObj);
        if (obj) {
            deleter(obj);
            HandleManager::getInstance()->removeHandle(hl);
            res = true;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
