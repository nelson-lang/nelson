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
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "FileSystemWrapper.hpp"
#include "CopyFile.hpp"
#include "Error.hpp"
#include "PredefinedErrorMessages.hpp"
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
            raiseError(
                L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
        raiseError(L"Nelson:files_folders_functions:ERROR_FILE_SOURCE_DOES_NOT_EXIST",
            ERROR_FILE_SOURCE_DOES_NOT_EXIST);
    }
    FileSystemWrapper::Path srcPath = srcFile;
    FileSystemWrapper::Path destPath = destFileOrDirectory;

    bool isDir = FileSystemWrapper::Path::is_directory(destFileOrDirectory, permissionDenied);
    if (permissionDenied) {
        raiseError(
            L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
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
                = fmt::format(_W("Source directory {0} does not exist or is not a directory."),
                    sourceDir.wstring());
            return false;
        }
    }
    if (!FileSystemWrapper::Path::is_directory(sourceDir)) {
        if (!FileSystemWrapper::Path::create_directory(destinationDir)) {
            if (!bForce) {
                errorMessage = fmt::format(
                    _W("Cannot create destination directory {0}"), destinationDir.wstring());
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
            raiseError(
                L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
        raiseError(L"Nelson:files_folders_functions:ERROR_DIRECTORY_SOURCE_DOES_NOT_EXIST",
            ERROR_DIRECTORY_SOURCE_DOES_NOT_EXIST);
    }
    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
        if (permissionDenied) {
            raiseError(
                L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
        raiseError(L"Nelson:files_folders_functions:ERROR_DIRECTORY_DESTINATION_DOES_NOT_EXIST",
            ERROR_DIRECTORY_DESTINATION_DOES_NOT_EXIST);
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
                raiseError(L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED",
                    ERROR_PERMISSION_DENIED);
            }
            raiseError(
                L"Nelson:files_folders_functions:ERROR_A_CELL_OF_EXISTING_FILENAMES_EXPECTED",
                ERROR_A_CELL_OF_EXISTING_FILENAMES_EXPECTED);
        }
    }

    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
        if (permissionDenied) {
            raiseError(
                L"Nelson:files_folders_functions:ERROR_PERMISSION_DENIED", ERROR_PERMISSION_DENIED);
        }
        raiseError(L"Nelson:files_folders_functions:ERROR_DIRECTORY_DESTINATION_DOES_NOT_EXIST",
            ERROR_DIRECTORY_DESTINATION_DOES_NOT_EXIST);
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
