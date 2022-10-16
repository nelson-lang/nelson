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
#include <filesystem>
#include <string>
#include <cstring>
#ifdef __GNUC__
#include "characters_encoding.hpp"
#endif
//=============================================================================
namespace Nelson {
inline std::filesystem::path
createFileSystemPath(const std::wstring& filename)
{
#ifdef __GNUC__
    // see why here https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95048 :(
    return std::filesystem::path(wstring_to_utf8(filename));
#else
    return std::filesystem::path(filename);
#endif
}
//=============================================================================
#ifdef _MSC_VER
inline std::wstring
uniformizeDriveLetter(const std::wstring& filename)
{
    std::wstring asWstring(filename);
    if (asWstring.length() > 1 && asWstring[1] == L':') {
        asWstring[0] = towupper(asWstring[0]);
    }
    return asWstring;
}
#endif
//=============================================================================
inline std::wstring
convertFileSytemPathToWString(const std::filesystem::path& path)
{
#ifdef __GNUC__
    return utf8_to_wstring(path.string());
#else
#ifdef _MSC_VER
    return uniformizeDriveLetter(path.wstring());
#else
    return path.wstring();
#endif
#endif
}
//=============================================================================
inline std::wstring
convertFileSytemPathToGenericWString(const std::filesystem::path& path)
{
#ifdef __GNUC__
    return utf8_to_wstring(path.generic_string());
#else
#ifdef _MSC_VER
    return uniformizeDriveLetter(path.generic_wstring());
#else
    return path.wstring();
#endif
#endif
}
//=============================================================================
inline std::filesystem::path
uniquePath()
{
    std::filesystem::path tempFilePath = std::filesystem::temp_directory_path();
#ifdef _MSC_VER
    char name[L_tmpnam_s];
    tmpnam_s(name, L_tmpnam_s);
    tempFilePath /= std::string(name);
#else
    FILE* uid_file;
    std::string template_name = tempFilePath.string() + "/NELSON_XXXXXX";
    char template_char[FILENAME_MAX * 2];
    strncpy(template_char, template_name.c_str(), template_name.length());
    int temp_fd = mkstemp(template_char);
    uid_file = fdopen(temp_fd, "w");
    if (uid_file) {
        tempFilePath = std::string(template_char);
        fclose(uid_file);
    } else {
        tempFilePath /= "/NELSON_XXXXXX";
    }
#endif
    return tempFilePath;
}
//=============================================================================
inline bool
isFile(const std::filesystem::path& filePath)
{
    bool bIsFile;
    try {
        bIsFile = std::filesystem::exists(filePath) && !std::filesystem::is_directory(filePath);
    } catch (const std::filesystem::filesystem_error&) {
        bIsFile = false;
    }
    return bIsFile;
}
//=============================================================================
inline bool
isFile(const std::wstring& filename)
{
    std::filesystem::path filePath = createFileSystemPath(filename);
    return isFile(filePath);
}
//=============================================================================
inline bool
isDirectory(const std::filesystem::path& filePath)
{
    bool bIsDirectory;
    try {
        bIsDirectory = std::filesystem::exists(filePath) && std::filesystem::is_directory(filePath);
    } catch (const std::filesystem::filesystem_error&) {
        bIsDirectory = false;
    }
    return bIsDirectory;
}
//=============================================================================
inline bool
isDirectory(const std::wstring& filename)
{
    std::filesystem::path filePath = createFileSystemPath(filename);
    return isDirectory(filePath);
}
//=============================================================================
inline bool
updateFilePermissionsToWrite(const std::filesystem::path& filePath)
{
    try {
        std::filesystem::permissions(filePath,
            std::filesystem::perms::owner_write | std::filesystem::perms::group_write
                | std::filesystem::perms::others_write,
            std::filesystem::perm_options::add);
        if (isDirectory(filePath)) {
            for (std::filesystem::recursive_directory_iterator p(filePath), end; p != end; ++p) {
                updateFilePermissionsToWrite(p->path());
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
    std::filesystem::path filePath = createFileSystemPath(folderName);
    return updateFilePermissionsToWrite(filePath);
}
//=============================================================================
inline bool
isEquivalentPath(const std::wstring& p1, const std::wstring& p2)
{
    std::filesystem::path path1 = createFileSystemPath(p1);
    std::filesystem::path path2 = createFileSystemPath(p2);
    bool res = false;
    try {
        res = std::filesystem::equivalent(path1, path2);
    } catch (const std::filesystem::filesystem_error&) {
        res = (p1.compare(p2) == 0);
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
