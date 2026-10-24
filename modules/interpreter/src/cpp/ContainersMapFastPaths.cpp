//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "ContainersMapFastPaths.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ContainersMapObject*
charKeyMapNativeStorage(const ArrayOf& r);
//=============================================================================
ContainersMapObject*
containersMapNativeStorage(const ArrayOf& r, ClassdefHandleObject** objectOut)
{
    if (!r.isScalar()) {
        return nullptr;
    }
    auto* object = dynamic_cast<ClassdefHandleObject*>(r.asHandleScalar());
    if (object == nullptr || !object->hasStoredProperty("NativeIndexMap")) {
        return nullptr;
    }
    ArrayOf native = object->getProperty("NativeIndexMap");
    if (!native.isHandle() || !native.isScalar()) {
        return nullptr;
    }
    auto* nativeMap = dynamic_cast<ContainersMapObject*>(native.asHandleScalar());
    if (nativeMap != nullptr && objectOut != nullptr) {
        *objectOut = object;
    }
    return nativeMap;
}
//=============================================================================
bool
containersMapCharKey(const ArrayOfVector& subsindices, std::string& key)
{
    if (subsindices.size() != 1 || !subsindices.front().isCell()
        || subsindices.front().elementCount() != 1) {
        return false;
    }
    const ArrayOf& element = static_cast<const ArrayOf*>(subsindices.front().dataPointer())[0];
    if (!element.isRowVectorCharacterArray()) {
        return false;
    }
    key = element.asUtf8String();
    return true;
}
//=============================================================================
bool
tryContainersMapCount(const ArrayOf& r, ArrayOfVector& result)
{
    ContainersMapObject* nativeMap = charKeyMapNativeStorage(r);
    if (nativeMap == nullptr) {
        return false;
    }
    result.push_back(ArrayOf::doubleScalar(static_cast<double>(nativeMap->count())));
    return true;
}
//=============================================================================
bool
tryContainersMapIsKey(const ArrayOf& r, const ArrayOf& key, ArrayOfVector& result)
{
    // the method accepts any char row vector, including 1x0 but not 0x0
    if (!key.isCharacterArray() || key.isSparse() || !key.dimensions().isRowVector()) {
        return false;
    }
    ContainersMapObject* nativeMap = charKeyMapNativeStorage(r);
    if (nativeMap == nullptr) {
        return false;
    }
    result.push_back(ArrayOf::logicalScalar(nativeMap->isKey(key.asUtf8String())));
    return true;
}
//=============================================================================
ContainersMapObject*
charKeyMapNativeStorage(const ArrayOf& r)
{
    // exact class: a subclass may override the methods
    if (!r.isHandle() || r.handleCategory() != NLS_HANDLE_CONTAINERS_MAP_CATEGORY_STR) {
        return nullptr;
    }
    ClassdefHandleObject* object = nullptr;
    ContainersMapObject* nativeMap = containersMapNativeStorage(r, &object);
    // numeric-key maps also use native storage; the char-key shortcuts
    // (isKey with a char key) only apply when the key type really is char
    if (nativeMap == nullptr || object == nullptr
        || object->getProperty("KeyTypeValue").asUtf8String() != "char") {
        return nullptr;
    }
    return nativeMap;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
