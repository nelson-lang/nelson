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
#include <string>
#include "nlsModules_manager_exports.h"
//=============================================================================
namespace Nelson {
NLSMODULES_MANAGER_IMPEXP std::wstring
FindDynamicLibraryName(const std::wstring& directoryName, const std::wstring& initialLibraryName,
    bool bCaseSensitive = false);
}
//=============================================================================
