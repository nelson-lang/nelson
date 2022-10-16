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
#include <boost/filesystem.hpp>
#include <boost/filesystem/path.hpp>
//=============================================================================
#include <string>
#include <cstring>
#ifdef __GNUC__
#include "characters_encoding.hpp"
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
inline boost::filesystem::path
createFileSystemPath(const std::wstring& filename)
{
    return boost::filesystem::path(filename);
}
//=============================================================================
inline std::wstring
convertFileSytemPathToWString(const boost::filesystem::path& path)
{
    return path.wstring();
}
//=============================================================================
inline std::wstring
convertFileSytemPathToGenericWString(const boost::filesystem::path& path)
{
    return path.generic_wstring();
}
//=============================================================================
inline bool
isFile(const boost::filesystem::path& filePath, bool& permissionDenied)
{
    bool bIsFile;
    permissionDenied = false;
    try {
        bIsFile = boost::filesystem::exists(filePath) && !boost::filesystem::is_directory(filePath);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            permissionDenied = true;
        }
        bIsFile = false;
    }
    return bIsFile;
}
//=============================================================================
inline bool
isFile(const boost::filesystem::path& filePath)
{
    bool permissionDenied;
    return isFile(filePath, permissionDenied);
}
//=============================================================================
inline bool
isFile(const std::wstring& filename)
{
    boost::filesystem::path filePath = createFileSystemPath(filename);
    return isFile(filePath);
}
//=============================================================================
inline bool
isDirectory(const boost::filesystem::path& filePath, bool& permissionDenied)
{
    bool bIsDirectory;
    permissionDenied = false;
    try {
        bIsDirectory
            = boost::filesystem::exists(filePath) && boost::filesystem::is_directory(filePath);
    } catch (const boost::filesystem::filesystem_error& e) {
        if (e.code() == boost::system::errc::permission_denied) {
            permissionDenied = true;
        }
        bIsDirectory = false;
    }
    return bIsDirectory;
}
//=============================================================================
inline bool
isDirectory(const boost::filesystem::path& filePath)
{
    bool permissionDenied;
    return isDirectory(filePath, permissionDenied);
}
//=============================================================================
inline bool
isDirectory(const std::wstring& filename)
{
    boost::filesystem::path filePath = createFileSystemPath(filename);
    return isDirectory(filePath);
}
//=============================================================================
inline bool
updateFilePermissionsToWrite(const boost::filesystem::path& filePath)
{
    try {
        boost::filesystem::permissions(filePath,
            boost::filesystem::perms::owner_write | boost::filesystem::perms::group_write
                | boost::filesystem::perms::others_write | boost::filesystem::add_perms);
        if (isDirectory(filePath)) {
            for (boost::filesystem::recursive_directory_iterator p(filePath), end; p != end; ++p) {
                updateFilePermissionsToWrite(p->path());
            }
        }
        return true;
    } catch (const boost::filesystem::filesystem_error&) {
    }
    return false;
}
//=============================================================================
inline bool
updateFilePermissionsToWrite(const std::wstring& folderName)
{
    boost::filesystem::path filePath = createFileSystemPath(folderName);
    return updateFilePermissionsToWrite(filePath);
}
//=============================================================================
inline bool
isEquivalentPath(const std::wstring& p1, const std::wstring& p2)
{
    boost::filesystem::path path1 = createFileSystemPath(p1);
    boost::filesystem::path path2 = createFileSystemPath(p2);
    bool res = false;
    try {
        res = boost::filesystem::equivalent(path1, path2);
    } catch (const boost::filesystem::filesystem_error&) {
        res = (p1.compare(p2) == 0);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
