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
} // namespace Nelson
//=============================================================================
