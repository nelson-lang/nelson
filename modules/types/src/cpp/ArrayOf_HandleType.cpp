//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ArrayOf.hpp"
#include "Data.hpp"
#include "HandleManager.hpp"
#include "HandleState.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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
ArrayOf::handle(nelson_handle hl)
{
    Dimensions dims(1, 1);
    auto* ref = static_cast<HandleRef*>(
        allocateArrayOf(NLS_HANDLE, dims.elementCount(), stringVector(), false));
    ref[0] = HandleManager::getInstance()->getHandleRef(hl);
    ArrayOf result(NLS_HANDLE, dims, ref);
    if (ref[0]) {
        result.dp->setClassTypeName(ref[0]->category());
    }
    return result;
}
//=============================================================================
ArrayOf
ArrayOf::handle(HandleGenericObject* ptr)
{
    Dimensions dims(1, 1);
    auto* ref = static_cast<HandleRef*>(
        allocateArrayOf(NLS_HANDLE, dims.elementCount(), stringVector(), false));
    ref[0] = HandleManager::getInstance()->addHandleRef(ptr);
    ArrayOf result(NLS_HANDLE, dims, ref);
    if (ref[0]) {
        result.dp->setClassTypeName(ref[0]->category());
    }
    return result;
}
//=============================================================================
ArrayOf
ArrayOf::emptyHandle(const std::string& className, Dimensions& dim)
{
    if (dim.elementCount() != 0) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    auto* qp = static_cast<HandleRef*>(
        allocateArrayOf(NLS_HANDLE, dim.elementCount(), stringVector(), false));
    ArrayOf result(NLS_HANDLE, dim, qp, false);
    result.dp->setClassTypeName(className.empty() ? std::string(NLS_HANDLE_STR) : className);
    return result;
}
//=============================================================================
ArrayOf
ArrayOf::invalidHandle(const std::string& className, Dimensions& dim)
{
    auto* qp = static_cast<HandleRef*>(
        allocateArrayOf(NLS_HANDLE, dim.elementCount(), stringVector(), false));
    const std::string category = className.empty() ? std::string(NLS_HANDLE_STR) : className;
    for (indexType k = 0; k < dim.elementCount(); ++k) {
        qp[k] = HandleManager::getInstance()->addInvalidHandleRef(category);
    }
    ArrayOf result(NLS_HANDLE, dim, qp, false);
    result.dp->setClassTypeName(category);
    return result;
}
//=============================================================================
HandleGenericObject*
ArrayOf::asHandleScalar() const
{
    if (!isHandle()) {
        Error(L"Nelson:types:handleExpected:expectedAHandleScalar", L"Expected a handle scalar.");
    }
    if (!isScalar()) {
        Error(L"Nelson:types:handleExpected:expectedAHandleScalar", L"Expected a handle scalar.");
    }
    const auto* qp = static_cast<const HandleRef*>(dp->data());
    if (qp == nullptr) {
        Error(L"Nelson:types:handleExpected:expectedAValidHandle", L"Expected a valid handle.");
    }
    return qp[0] ? qp[0]->object() : nullptr;
}
//=============================================================================
bool
ArrayOf::isHandleProperty(const std::string& methodName) const
{
    return isHandleProperty(utf8_to_wstring(methodName));
}
//=============================================================================
bool
ArrayOf::isHandleProperty(const std::wstring& propertyName) const
{
    if (!isHandle()) {
        Error(L"Nelson:types:handleExpected:expectedAHandle", L"Expected a handle.");
    }
    const auto* ptr = static_cast<const HandleRef*>(dataPointer());
    indexType nbElements = elementCount();
    bool isProperty = false;
    if (nbElements > 0) {
        for (indexType k = 0; k < nbElements; k++) {
            if (ptr[k]) {
                HandleGenericObject* hlObj = ptr[k]->object();
                if (!hlObj) {
                    continue;
                }
                isProperty = hlObj->isProperty(propertyName);
                if (!isProperty) {
                    return false;
                }
            }
        }
    }
    return isProperty;
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
        Error(L"Nelson:types:handleExpected:expectedAHandle", L"Expected a handle.");
    }
    const auto* ptr = static_cast<const HandleRef*>(dataPointer());
    indexType nbElements = elementCount();
    bool isMethod = false;
    if (nbElements > 0) {
        for (indexType k = 0; k < nbElements; k++) {
            if (ptr[k]) {
                HandleGenericObject* hlObj = ptr[k]->object();
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
ArrayOf::handleCategory() const
{
    if (!isHandle()) {
        Error(L"Nelson:types:handleExpected:expectedAHandle", L"Expected a handle.");
    }
    const auto* qp = static_cast<const HandleRef*>(dp->data());
    if (elementCount() == 0) {
        const std::string category = dp->classTypeName();
        return category.empty() || category == NLS_CLASS_ARRAY_STR ? NLS_HANDLE_STR : category;
    }
    if (qp == nullptr) {
        return NLS_HANDLE_STR;
    }
    indexType nbElements = elementCount();
    std::string category = NLS_HANDLE_STR;
    for (indexType k = 0; k < nbElements; k++) {
        if (qp[k]) {
            std::string current = qp[k]->category();
            if (category != current && current != "handle") {
                category = std::move(current);
            }
        }
    }
    return category;
}
//=============================================================================
std::string
ArrayOf::handleClassName() const
{
    if (!isHandle()) {
        Error(L"Nelson:types:handleExpected:expectedAHandle", L"Expected a handle.");
    }
    const auto* qp = static_cast<const HandleRef*>(dp->data());
    if (elementCount() == 0) {
        const std::string className = dp->classTypeName();
        return className.empty() || className == NLS_CLASS_ARRAY_STR ? NLS_HANDLE_STR : className;
    }
    if (qp == nullptr) {
        return NLS_HANDLE_STR;
    }
    if (qp[0]) {
        return qp[0]->className();
    }
    return NLS_HANDLE_STR;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
