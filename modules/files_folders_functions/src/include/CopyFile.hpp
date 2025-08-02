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
#include "Types.hpp"
#include "nlsFiles_folders_functions_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP bool
CopyFile(const std::wstring& srcFile, const std::wstring& destFileOrDirectory, bool bForce,
    std::wstring& message);
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP bool
CopyDirectory(
    const std::wstring& srcDir, const std::wstring& destDir, bool bForce, std::wstring& message);
NLSFILES_FOLDERS_FUNCTIONS_IMPEXP bool
CopyFiles(
    const wstringVector& srcFiles, const std::wstring& destDir, bool bForce, std::wstring& message);
} // namespace Nelson
//=============================================================================
