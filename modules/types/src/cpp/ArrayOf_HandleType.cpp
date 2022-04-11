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
ArrayOf::isHandleMethod(const std::wstring& methodName) const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->isMethod(methodName);
}
//=============================================================================
std::wstring
ArrayOf::getHandleCategory() const
{
    HandleGenericObject* obj = getContentAsHandleScalar();
    return obj->getCategory();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
