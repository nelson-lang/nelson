//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
ConstructBinariesPath(const std::wstring& modulerootpath);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructDynamicLibraryName(const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructDynamicLibraryFullname(
    const std::wstring& modulerootpath, const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructEtcName(const std::wstring& modulerootpath, const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructScriptName(const std::wstring& modulerootpath, const std::wstring& moduleshortname);
NLSMODULES_MANAGER_IMPEXP std::wstring
ConstructRootName(const std::wstring& modulerootpath, const std::wstring& moduleshortname);
} // namespace Nelson
//=============================================================================
