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
#include <string>
#include <cstring>
//=============================================================================
#include "FileSystemWrapper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline bool
isFile(const Nelson::FileSystemWrapper::Path& filePath, bool& permissionDenied)
{
    bool bIsFile;
    permissionDenied = false;
    try {
        bIsFile = filePath.exists() && !filePath.is_directory();
    } catch (const std::filesystem::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            permissionDenied = true;
        }
        bIsFile = false;
    }
    return bIsFile;
}
//=============================================================================
inline bool
isFile(const Nelson::FileSystemWrapper::Path& filePath)
{
    bool permissionDenied;
    return isFile(filePath, permissionDenied);
}
//=============================================================================
inline bool
isFile(const std::wstring& filename)
{
    Nelson::FileSystemWrapper::Path filePath(filename);
    return isFile(filePath);
}
//=============================================================================
inline bool
isDirectory(const Nelson::FileSystemWrapper::Path& filePath, bool& permissionDenied)
{
    bool bIsDirectory;
    permissionDenied = false;
    try {
        bIsDirectory = filePath.exists() && filePath.is_directory();
    } catch (const std::filesystem::filesystem_error& e) {
        if (e.code() == std::errc::permission_denied) {
            permissionDenied = true;
        }
        bIsDirectory = false;
    }
    return bIsDirectory;
}
//=============================================================================
inline bool
isDirectory(const Nelson::FileSystemWrapper::Path& filePath)
{
    bool permissionDenied;
    return isDirectory(filePath, permissionDenied);
}
//=============================================================================
inline bool
isDirectory(const std::wstring& filename)
{
    Nelson::FileSystemWrapper::Path filePath(filename);
    return isDirectory(filePath);
}
//=============================================================================
inline bool
updateFilePermissionsToWrite(const Nelson::FileSystemWrapper::Path& filePath)
{
    try {
        std::filesystem::permissions(filePath.native(),
            std::filesystem::perms::owner_write | std::filesystem::perms::group_write
                | std::filesystem::perms::others_write,
            std::filesystem::perm_options::add);
        if (isDirectory(filePath)) {
            for (std::filesystem::recursive_directory_iterator p(filePath.native()), end; p != end;
                 ++p) {
                updateFilePermissionsToWrite(Nelson::FileSystemWrapper::Path(p->path().native()));
            }
        }
        return true;
    } catch (const std::filesystem::filesystem_error&) {
    }
    return false;
}
//=============================================================================
inline bool
updateFilePermissionsToWrite(const std::wstring& folderName)
{
    Nelson::FileSystemWrapper::Path filePath(folderName);
    return updateFilePermissionsToWrite(filePath);
}
//=============================================================================
inline bool
isEquivalentPath(const std::wstring& p1, const std::wstring& p2)
{
    Nelson::FileSystemWrapper::Path path1(p1);
    Nelson::FileSystemWrapper::Path path2(p2);
    bool res = false;
    try {
        res = Nelson::FileSystemWrapper::Path::equivalent(path1, path2);
    } catch (const std::filesystem::filesystem_error&) {
        res = (p1.compare(p2) == 0);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
