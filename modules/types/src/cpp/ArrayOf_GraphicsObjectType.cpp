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
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GraphicsObjectClassNameResolver.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ArrayOf::isGraphicsObject() const
{
    return dp && dp->dataClass == NLS_GO_HANDLE;
}
//=============================================================================
std::string
ArrayOf::graphicsObjectClassName() const
{
    if (!isGraphicsObject()) {
        Error(
            L"Nelson:types:handleExpected:expectedGraphicsObject", L"Expected an graphics object.");
    }
    const std::string className = dp->classTypeName();
    const bool hasGenericClassName
        = className.empty() || className == NLS_CLASS_ARRAY_STR || className == NLS_GO_HANDLE_STR;
    if (!hasGenericClassName) {
        return className;
    }
    if (!isScalar()) {
        return NLS_GRAPHICS_OBJECT_CLASS_STR;
    }
    const auto* ptr = static_cast<const nelson_handle*>(dataPointer());
    if (ptr != nullptr) {
        std::string resolvedClassName
            = resolveGraphicsObjectClassName(static_cast<go_handle>(ptr[0]));
        if (!resolvedClassName.empty()) {
            return resolvedClassName;
        }
    }
    return NLS_GRAPHICS_OBJECT_CLASS_STR;
}
//=============================================================================
void
ArrayOf::setGraphicsObjectClassName(const std::string& className)
{
    if (!isGraphicsObject()) {
        Error(
            L"Nelson:types:handleExpected:expectedGraphicsObject", L"Expected an graphics object.");
    }
    ensureSingleOwner();
    dp->setClassTypeName(className.empty() ? NLS_GRAPHICS_OBJECT_CLASS_STR : className);
}
//=============================================================================
void
ArrayOf::refreshGraphicsObjectClassName()
{
    if (!isGraphicsObject()) {
        return;
    }
    if (elementCount() == 0) {
        return;
    }
    const auto* ptr = static_cast<const nelson_handle*>(dataPointer());
    if (ptr == nullptr) {
        setGraphicsObjectClassName(NLS_GRAPHICS_OBJECT_CLASS_STR);
        return;
    }
    std::string commonClassName;
    for (indexType k = 0; k < elementCount(); ++k) {
        std::string currentClassName
            = resolveGraphicsObjectClassName(static_cast<go_handle>(ptr[k]));
        if (currentClassName.empty()) {
            currentClassName = NLS_GRAPHICS_OBJECT_CLASS_STR;
        }
        if (commonClassName.empty()) {
            commonClassName = std::move(currentClassName);
        } else if (commonClassName != currentClassName) {
            commonClassName = NLS_GRAPHICS_OBJECT_CLASS_STR;
            break;
        }
    }
    setGraphicsObjectClassName(commonClassName);
}
//=============================================================================
ArrayOf
ArrayOf::graphicsObject(go_handle graphicsObjectHandle)
{
    nelson_handle* ptr = static_cast<nelson_handle*>(ArrayOf::allocateArrayOf(NLS_GO_HANDLE, 1));
    Dimensions dims(1, 1);
    ptr[0] = (nelson_handle)graphicsObjectHandle;
    ArrayOf res = ArrayOf(NLS_GO_HANDLE, dims, (void*)ptr);
    return res;
}
//=============================================================================
ArrayOf
ArrayOf::graphicsObject(go_handle graphicsObjectHandle, const std::string& className)
{
    ArrayOf result = graphicsObject(graphicsObjectHandle);
    result.dp->setClassTypeName(className.empty() ? std::string(NLS_GO_HANDLE_STR) : className);
    return result;
}
//=============================================================================
ArrayOf
ArrayOf::graphicsObject(const std::vector<go_handle>& graphicsObjectHandles,
    const Dimensions& dimensions, const std::string& className)
{
    if (dimensions.elementCount() != graphicsObjectHandles.size()) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    auto* ptr = static_cast<nelson_handle*>(ArrayOf::allocateArrayOf(
        NLS_GO_HANDLE, graphicsObjectHandles.size(), stringVector(), true));
    for (size_t k = 0; k < graphicsObjectHandles.size(); ++k) {
        ptr[k] = static_cast<nelson_handle>(graphicsObjectHandles[k]);
    }
    ArrayOf result(NLS_GO_HANDLE, dimensions, ptr);
    result.dp->setClassTypeName(className.empty() ? std::string(NLS_GO_HANDLE_STR) : className);
    return result;
}
//=============================================================================
ArrayOf
ArrayOf::emptyGraphicsObject(const std::string& className, Dimensions& dim)
{
    if (dim.elementCount() != 0) {
        Error(L"Nelson:types:invalidDimensions:invalidDimensions", L"Invalid dimensions.");
    }
    auto* ptr = static_cast<nelson_handle*>(
        ArrayOf::allocateArrayOf(NLS_GO_HANDLE, dim.elementCount(), stringVector(), false));
    ArrayOf result(NLS_GO_HANDLE, dim, ptr, false);
    result.dp->setClassTypeName(className.empty() ? std::string(NLS_GO_HANDLE_STR) : className);
    return result;
}
//=============================================================================
go_handle
ArrayOf::asGraphicsObjectScalar() const
{
    if (!isGraphicsObject()) {
        Error(
            L"Nelson:types:handleExpected:expectedGraphicsObject", L"Expected an graphics object.");
    }
    if (!isScalar()) {
        Error(L"Nelson:types:handleExpected:expectedGraphicsObjectScalar",
            L"Expected an graphics object scalar.");
    }
    nelson_handle* qp = (nelson_handle*)dataPointer();
    if (qp == nullptr) {
        Error(L"Nelson:types:handleExpected:expectedAValidHandle", L"Expected a valid handle.");
    }
    return (go_handle)(qp[0]);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
