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
#include "nlsNelson_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSNELSON_MANAGER_IMPEXP std::wstring
getDialogDirectory(const std::wstring& key);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP bool
setDialogDirectory(const std::wstring& key, const std::wstring& path);
//=============================================================================
NLSNELSON_MANAGER_IMPEXP std::wstring
resolveDialogDirectory(
    const std::wstring& key, const std::wstring& explicitPath, const std::wstring& fallbackPath);
//=============================================================================
} // namespace Nelson
//=============================================================================
