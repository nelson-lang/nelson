//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include "FileSystemWrapper.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
bool
CopyFile(const std::wstring& srcFile, const std::wstring& destFileOrDirectory, bool bForce,
    std::wstring& message)
{
    bool bRes = false;
    message = L"";
    bool permissionDenied;
    if (!FileSystemWrapper::Path::is_regular_file(srcFile, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("File source does not exist."));
    }
    FileSystemWrapper::Path srcPath = srcFile;
    FileSystemWrapper::Path destPath = destFileOrDirectory;

    bool isDir = FileSystemWrapper::Path::is_directory(destFileOrDirectory, permissionDenied);
    if (permissionDenied) {
        Error(_W("Permission denied."));
    }
    if (isDir) {
        destPath = destPath / srcPath.filename();
    }
    std::string errorMessage;
    FileSystemWrapper::Path::copy_file(srcPath, destPath, errorMessage);
    if (errorMessage.empty()) {
        bRes = true;
    } else {
        bRes = false;
        message = utf8_to_wstring(errorMessage);
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
copyDirectoryRecursively(const FileSystemWrapper::Path& sourceDir,
    const FileSystemWrapper::Path& destinationDir, bool bForce, std::wstring& errorMessage)
{
    if (!FileSystemWrapper::Path::is_directory(sourceDir)) {
        if (!bForce) {
            errorMessage
                = fmt::sprintf(_W("Source directory %s does not exist or is not a directory."),
                    sourceDir.wstring());
            return false;
        }
    }
    if (!FileSystemWrapper::Path::is_directory(sourceDir)) {
        if (!FileSystemWrapper::Path::create_directory(destinationDir)) {
            if (!bForce) {
                errorMessage = fmt::sprintf(
                    _W("Cannot create destination directory %s"), destinationDir.wstring());
                return false;
            }
        }
    }

    std::wstring rootSrc = sourceDir.generic_wstring();
    for (const auto& dirEnt : nfs::recursive_directory_iterator { sourceDir.native() }) {
        const auto& path = dirEnt.path();
        std::wstring relativePathStr = path.generic_wstring();
        StringHelpers::replace_first(relativePathStr, rootSrc, L"");
        FileSystemWrapper::Path destPath = destinationDir.generic_path() / relativePathStr;
        std::string message;
        FileSystemWrapper::Path::copy(FileSystemWrapper::Path(path.wstring()), destPath, message);
        if (!errorMessage.empty()) {
            errorMessage = utf8_to_wstring(message);
            return false;
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
    if (!FileSystemWrapper::Path::is_directory(srcDir, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("Directory source does not exist."));
    }
    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
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
        if (!FileSystemWrapper::Path::is_regular_file(srcFile, permissionDenied)) {
            if (permissionDenied) {
                Error(_W("Permission denied."));
            }
            Error(_W("A cell of existing filenames expected."));
        }
    }

    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
        if (permissionDenied) {
            Error(_W("Permission denied."));
        }
        Error(_W("Directory destination does not exist."));
    }
    for (const auto& srcFile : srcFiles) {
        FileSystemWrapper::Path srcPath = srcFile;
        FileSystemWrapper::Path destPath = destDir;
        destPath = destPath / srcPath.filename();
        std::string errorMessage;
        bRes = FileSystemWrapper::Path::copy_file(srcPath, destPath, errorMessage);
        if (!bRes) {
            message = utf8_to_wstring(errorMessage);
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
