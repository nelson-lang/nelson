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
        bIsFile = filePath.is_regular_file();
    } catch (const nfs::filesystem_error& e) {
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
    } catch (const nfs::filesystem_error& e) {
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
#ifdef _WITH_BOOST_FILESYSTEM_
        nfs::permissions(filePath.native(),
            nfs::add_perms | nfs::owner_write | nfs::group_write | nfs::others_write);
#else
        nfs::permissions(filePath.native(),
            nfs::perms::owner_write | nfs::perms::group_write | nfs::perms::others_write,
            nfs::perm_options::add);
#endif
        if (isDirectory(filePath)) {
            for (nfs::recursive_directory_iterator p(filePath.native()), end; p != end; ++p) {
                updateFilePermissionsToWrite(Nelson::FileSystemWrapper::Path(p->path().native()));
            }
        }
        return true;
    } catch (const nfs::filesystem_error&) {
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
} // namespace Nelson
//=============================================================================
