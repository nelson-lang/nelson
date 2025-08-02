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
#include "nlsModules_manager_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructDynamicLibraryName(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructDynamicLibraryFullname(
    const std::wstring& moduleRootPath, const std::wstring& moduleshortname, bool isInternalModule);
//=============================================================================
} // namespace Nelson
//=============================================================================
