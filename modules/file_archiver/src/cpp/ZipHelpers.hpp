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
#include "Types.hpp"
#include "ZipArchive.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
normalizeZipPath(const std::wstring& path);
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath);
//=============================================================================
// Case-insensitive match where '*' is any run and '?' any one character.
bool
wildcardMatch(const std::wstring& name, const std::wstring& mask);
//=============================================================================
// ZIP-slip guard. Rejects backslashes, ':' (drive or stream), absolute paths and
// ".." components, then checks that the normalized destination stays under
// fullRootPath. On success sets resolvedPath and returns true.
bool
resolveSafeZipEntryPath(
    const std::wstring& fullRootPath, const std::wstring& entryName, std::wstring& resolvedPath);
//=============================================================================
// Same folder with symbolic links resolved (the disk writer refuses to write through links).
std::wstring
canonicalFolder(const std::wstring& folder);
//=============================================================================
// safePath (under fullRootPath) moved under canonicalRoot.
std::wstring
diskPath(const std::wstring& canonicalRoot, const std::wstring& fullRootPath,
    const std::wstring& safePath);
//=============================================================================
// True when a link named entryName pointing to target stays inside the extraction folder.
bool
linkStaysInside(const std::string& entryName, const std::string& target);
//=============================================================================
// Replaces each folder by its files and empty subfolders (an empty folder is kept itself).
wstringVector
expandFolders(const wstringVector& paths);
//=============================================================================
// Files and folders named by zip/tar arguments (wildcards, folders, root path), with their
// sorted entry names (folders without '/'); reproducible orders items and fixes the times.
void
collectArchiveItems(const wstringVector& names, const std::wstring& rootpath, bool reproducible,
    std::vector<ZipFileItem>& items, wstringVector& entrynames);
// Local file of the item named name (UTF-8 entry name), for error messages.
std::wstring
itemFilename(const std::vector<ZipFileItem>& items, const std::string& name);
//=============================================================================
} // namespace Nelson
//=============================================================================
