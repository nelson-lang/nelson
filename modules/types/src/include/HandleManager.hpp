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
#include "HandleState.hpp"
#include "Types.hpp"
#include "nlsTypes_exports.h"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
/**
 * Compatibility facade for handle registry operations.
 *
 * New code should use HandleRef-aware paths when possible. The manager keeps
 * the older numeric-handle API used by builtins and modules.
 */
class NLSTYPES_IMPEXP HandleManager
{
    //=============================================================================
public:
    static HandleManager*
    getInstance();
    void
    destroy();
    /**
     * Register a native object and return its numeric handle id.
     */
    nelson_handle
    addHandle(HandleGenericObject* ptr);
    /**
     * Register a native object and return the shared handle state.
     */
    HandleRef
    addHandleRef(HandleGenericObject* ptr);
    /**
     * Create an invalid handle state that keeps category information.
     */
    HandleRef
    addInvalidHandleRef(const std::string& categoryName);
    /**
     * Re-register an object under its former handle id (see HandleRegistry::adopt).
     */
    HandleRef
    adoptHandleRef(nelson_handle hl, HandleGenericObject* ptr);
    HandleRef
    getHandleRef(nelson_handle hl);
    bool
    removeHandle(nelson_handle hl);
    /**
     * Return the wrapped object for a valid handle id.
     */
    HandleGenericObject*
    pointer(nelson_handle hl);
    bool
    isValid(nelson_handle hl);
    /**
     * Return numeric ids for all live handles in a category.
     */
    std::vector<nelson_handle>
    getAllHandlesOfCategory(const std::string& category);
    /**
     * Return shared states for all live handles in a category.
     */
    std::vector<HandleRef>
    getAllHandleRefsOfCategory(const std::string& category);
    /**
     * Find a handle id by the wrapped native pointer value.
     */
    nelson_handle
    findByPointerValue(void* ptr);
    //=============================================================================
private:
    HandleManager();
    static HandleManager* m_pInstance;
    //=============================================================================
};
//=============================================================================
/**
 * Invalidate and delete all handles in A that match the requested category.
 *
 * The function accepts an optional deleter for native objects that require a
 * module-specific release path.
 */
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
        Error(L"Nelson:types:handleExpected", validHandleExpectedMsg);
        return false;
    }

    Dimensions dims = A.dimensions();
    HandleRef* qp = (HandleRef*)(A.dataPointer());
    size_t elementCount = static_cast<size_t>(dims.elementCount());
    bool res = false;

    for (size_t k = 0; k < elementCount; ++k) {
        HandleRef ref = qp[k];
        HandleGenericObject* hlObj = ref ? ref->object() : nullptr;
        if (!hlObj) {
            continue;
        }
        if (hlObj->category() != expectedCategory) {
            Error(L"Nelson:types:handleExpected", handleExpectedMsg);
            continue;
        }
        auto* obj = dynamic_cast<T*>(hlObj);
        if (obj) {
            res = ref->invalidateAs<T>(expectedCategory, deleter) || res;
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
