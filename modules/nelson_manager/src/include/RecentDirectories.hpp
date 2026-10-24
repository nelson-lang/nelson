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
#include <vector>
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSNELSON_MANAGER_IMPEXP std::vector<std::wstring>
getRecentDirectories();
//=============================================================================
NLSNELSON_MANAGER_IMPEXP bool
addRecentDirectory(const std::wstring& directory);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP std::wstring
getFirstValidRecentDirectory();
//=============================================================================
} // namespace Nelson
//=============================================================================
