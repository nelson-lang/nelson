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
#include <boost/filesystem.hpp>
#include <fmt/printf.h>
#include <fmt/format.h>
#include "CopyFile.hpp"
#include "Error.hpp"
#include "FileSystemHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
bool
CopyFile(const std::wstring& srcFile, const std::wstring& destFileOrDirectory, bool bForce,
    std::wstring& message)
{
    bool bRes = false;
    message = L"";
    bool permissionDenied;
    if (!isFile(srcFile, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("File source does not exist."));
    }
    boost::filesystem::path srcPath = srcFile;
    boost::filesystem::path destPath = destFileOrDirectory;

    bool isDir = isDirectory(destFileOrDirectory, permissionDenied);
    if (permissionDenied) {
        Error(_W("Permission denied."));
    }
    if (isDir) {
        destPath = destPath / srcPath.filename();
    }
    try {
        boost::filesystem::copy_file(
            srcPath, destPath, boost::filesystem::copy_option::overwrite_if_exists);
        bRes = true;
    } catch (const boost::filesystem::filesystem_error& e) {
        bRes = false;
        boost::system::error_code error_code = e.code();
        message = utf8_to_wstring(error_code.message());
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
copyDirectoryRecursively(const boost::filesystem::path& sourceDir,
    const boost::filesystem::path& destinationDir, bool bForce, std::wstring& errorMessage)
{
    if (!boost::filesystem::exists(sourceDir) || !boost::filesystem::is_directory(sourceDir)) {
        if (!bForce) {
            errorMessage
                = fmt::sprintf(_W("Source directory %s does not exist or is not a directory."),
                    sourceDir.wstring());
            return false;
        }
    }
    if (!boost::filesystem::exists(sourceDir) || !boost::filesystem::is_directory(sourceDir)) {
        if (!boost::filesystem::create_directory(destinationDir)) {
            if (!bForce) {
                errorMessage = fmt::sprintf(
                    _W("Cannot create destination directory %s"), destinationDir.wstring());
                return false;
            }
        }
    }

    std::wstring rootSrc = sourceDir.generic_wstring();
    for (const auto& dirEnt : boost::filesystem::recursive_directory_iterator { sourceDir }) {
        const auto& path = dirEnt.path();
        std::wstring relativePathStr = path.generic_wstring();
        boost::replace_first(relativePathStr, rootSrc, L"");
        try {
            boost::filesystem::path destPath = destinationDir.generic_path() / relativePathStr;
            boost::filesystem::copy(path, destPath);
        } catch (const boost::filesystem::filesystem_error& e) {
            if (!bForce) {
                boost::system::error_code error_code = e.code();
                errorMessage = utf8_to_wstring(error_code.message());
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
    bool permissionDenied;
    if (!isDirectory(srcDir, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("Directory source does not exist."));
    }
    if (!isDirectory(destDir, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
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
    bool permissionDenied;
    for (const auto& srcFile : srcFiles) {
        if (!isFile(srcFile, permissionDenied)) {
            if (permissionDenied) {
                Error(_W("Permission denied."));
            }
            Error(_W("A cell of existing filenames expected."));
        }
    }

    if (!isDirectory(destDir, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("Directory destination does not exist."));
    }
    for (const auto& srcFile : srcFiles) {
        boost::filesystem::path srcPath = srcFile;
        boost::filesystem::path destPath = destDir;
        destPath = destPath / srcPath.filename();
        try {
            boost::filesystem::copy_file(srcPath, destPath);
            bRes = true;
        } catch (const boost::filesystem::filesystem_error& e) {
            bRes = false;
            boost::system::error_code error_code = e.code();
            message = utf8_to_wstring(error_code.message());
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
