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
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
normalizeZipPath(const std::wstring& path);
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath);
//=============================================================================
// ZIP-slip guard. Rejects backslashes, ':' (drive or stream), absolute paths and
// ".." components, then checks that the normalized destination stays under
// fullRootPath. On success sets resolvedPath and returns true.
bool
resolveSafeZipEntryPath(
    const std::wstring& fullRootPath, const std::wstring& entryName, std::wstring& resolvedPath);
//=============================================================================
} // namespace Nelson
//=============================================================================
