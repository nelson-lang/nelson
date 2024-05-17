//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleManager.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isHandle() const
{
    if (dp) {
        return (dp->dataClass == NLS_HANDLE);
    }
    return false;
}
//=============================================================================
ArrayOf
ArrayOf::handleConstructor(nelson_handle hl)
{
    nelson_handle* ptrObject = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
    Dimensions dims(1, 1);
    ptrObject[0] = hl;
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
ArrayOf
ArrayOf::handleConstructor(HandleGenericObject* ptr)
{
    nelson_handle* ptrObject = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_HANDLE, 1, stringVector(), false));
    Dimensions dims(1, 1);
    ptrObject[0] = HandleManager::getInstance()->addHandle(ptr);
    return ArrayOf(NLS_HANDLE, dims, (void*)ptrObject);
}
//=============================================================================
HandleGenericObject*
ArrayOf::getContentAsHandleScalar() const
{
    if (!isHandle()) {
        Error(_W("Expected a handle scalar."));
    }
    if (!isScalar()) {
        Error(_W("Expected a handle scalar."));
    }
    auto* qp = (nelson_handle*)dp->getData();
    if (qp == nullptr) {
        Error(_W("Expected a valid handle."));
    }
    nelson_handle hl = 0L;
    if (qp != nullptr) {
        hl = (*qp);
    }
    return HandleManager::getInstance()->getPointer(hl);
}
//=============================================================================
bool
ArrayOf::isHandleProperty(const std::wstring& propertyName) const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->isProperty(propertyName);
}
//=============================================================================
bool
ArrayOf::isHandleMethod(const std::string& methodName) const
{
    return isHandleMethod(utf8_to_wstring(methodName));
}
//=============================================================================
bool
ArrayOf::isHandleMethod(const std::wstring& methodName) const
{
    if (!isHandle()) {
        Error(_W("Expected a handle."));
    }
    auto* ptr = (nelson_handle*)getDataPointer();
    indexType nbElements = getElementCount();
    bool isMethod = false;
    if (nbElements > 0) {
        for (indexType k = 0; k < nbElements; k++) {
            if (ptr[k]) {
                HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(ptr[k]);
                if (!hlObj) {
                    continue;
                }
                isMethod = hlObj->isMethod(methodName);
                if (!isMethod) {
                    return false;
                }
            }
        }
    }
    return isMethod;
}
//=============================================================================
std::string
ArrayOf::getHandleCategory() const
{
    if (!isHandle()) {
        Error(_W("Expected a handle."));
    }
    auto* qp = (nelson_handle*)dp->getData();
    if (qp == nullptr) {
        return NLS_HANDLE_STR;
    }
    indexType nbElements = getElementCount();
    std::string category = NLS_HANDLE_STR;
    /* handle can be 'handle' or another type but not mixed */
    for (indexType k = 0; k < nbElements; k++) {
        nelson_handle hl = qp[k];
        HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
        if (hlObj != nullptr) {
            std::string current = hlObj->getCategory();
            if (category != current && current != "handle") {
                category = std::move(current);
            }
        }
    }
    return category;
}
//=============================================================================
std::string
ArrayOf::getHandleClassName() const
{
    if (!isHandle()) {
        Error(_W("Expected a handle."));
    }
    auto* qp = (nelson_handle*)dp->getData();
    if (qp == nullptr) {
        return NLS_HANDLE_STR;
    }
    nelson_handle hl = qp[0];
    HandleGenericObject* hlObj = HandleManager::getInstance()->getPointer(hl);
    if (hlObj) {
        return hlObj->getClassName();
    }
    return NLS_HANDLE_STR;
}
//=============================================================================

} // namespace Nelson
//=============================================================================
