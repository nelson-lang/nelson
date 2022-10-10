//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <boost/algorithm/string/replace.hpp>
#include <filesystem>
#include <fmt/printf.h>
#include <fmt/format.h>
#include "CopyFile.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "IsFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
bool
CopyFile(const std::wstring& srcFile, const std::wstring& destFileOrDirectory, bool bForce,
    std::wstring& message)
{
    bool bRes = false;
    message = L"";
    if (!IsFile(srcFile)) {
        Error(_W("File source does not exist."));
    }
    std::filesystem::path srcPath = srcFile;
    std::filesystem::path destPath = destFileOrDirectory;
    if (IsDirectory(destFileOrDirectory)) {
        destPath = destPath / srcPath.filename();
    }
    try {
        std::filesystem::copy_file(
            srcPath, destPath, std::filesystem::copy_options::overwrite_existing);
        bRes = true;
    } catch (const std::filesystem::filesystem_error& e) {
        bRes = false;
        message = utf8_to_wstring(e.what());
    }
    if (bForce) {
        if (!bRes) {
            bRes = true;
            message = L"";
        }
    }
    return bRes;
}
//=============================================================================
static bool
copyDirectoryRecursively(const std::filesystem::path& sourceDir,
    const std::filesystem::path& destinationDir, bool bForce, std::wstring& errorMessage)
{
    if (!std::filesystem::exists(sourceDir) || !std::filesystem::is_directory(sourceDir)) {
        if (!bForce) {
            errorMessage
                = fmt::sprintf(_W("Source directory %s does not exist or is not a directory."),
                    sourceDir.wstring());
            return false;
        }
    }
    if (!std::filesystem::exists(sourceDir) || !std::filesystem::is_directory(sourceDir)) {
        if (!std::filesystem::create_directory(destinationDir)) {
            if (!bForce) {
                errorMessage = fmt::sprintf(
                    _W("Cannot create destination directory %s"), destinationDir.wstring());
                return false;
            }
        }
    }

    std::wstring rootSrc = sourceDir.generic_wstring();
    for (const auto& dirEnt : std::filesystem::recursive_directory_iterator { sourceDir }) {
        const auto& path = dirEnt.path();
        std::wstring relativePathStr = path.generic_wstring();
        boost::replace_first(relativePathStr, rootSrc, L"");
        try {
            std::filesystem::path destPath
                = std::filesystem::path(destinationDir.generic_wstring()) / relativePathStr;
            std::filesystem::copy(path, destPath);
        } catch (const std::filesystem::filesystem_error& e) {
            if (!bForce) {
                errorMessage = utf8_to_wstring(e.what());
                return false;
            }
        }
    }
    return true;
}
//=============================================================================
bool
CopyDirectory(
    const std::wstring& srcDir, const std::wstring& destDir, bool bForce, std::wstring& message)
{
    message = L"";
    if (!IsDirectory(srcDir)) {
        Error(_W("Directory source does not exist."));
    }
    if (!IsDirectory(destDir)) {
        Error(_W("Directory destination does not exist."));
    }

    return copyDirectoryRecursively(srcDir, destDir, bForce, message);
}
//=============================================================================
bool
CopyFiles(
    const wstringVector& srcFiles, const std::wstring& destDir, bool bForce, std::wstring& message)
{
    bool bRes = false;
    message = L"";
    for (const auto& srcFile : srcFiles) {
        if (!IsFile(srcFile)) {
            Error(_W("A cell of existing filenames expected."));
        }
    }
    if (!IsDirectory(destDir)) {
        Error(_W("Directory destination does not exist."));
    }
    for (const auto& srcFile : srcFiles) {
        std::filesystem::path srcPath = srcFile;
        std::filesystem::path destPath = destDir;
        destPath = destPath / srcPath.filename();
        try {
            std::filesystem::copy_file(srcPath, destPath);
            bRes = true;
        } catch (const std::filesystem::filesystem_error& e) {
            bRes = false;
            message = utf8_to_wstring(e.what());
        }
        if (bForce) {
            if (!bRes) {
                bRes = true;
                message.clear();
            }
        } else {
            if (!bRes) {
                return bRes;
            }
        }
    }
    return bRes;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
