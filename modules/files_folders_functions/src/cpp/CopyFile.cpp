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
#include "ErrorID.hpp"
#include "i18n.hpp"
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
    if (!FileSystemWrapper::Path::is_regular_file(srcFile, permissionDenied)) {
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        Error(L"Nelson:files_folders_functions:fileSourceDoesNotExist",
            L"File source does not exist.");
    }
    FileSystemWrapper::Path srcPath = srcFile;
    FileSystemWrapper::Path destPath = destFileOrDirectory;

    bool isDir = FileSystemWrapper::Path::is_directory(destFileOrDirectory, permissionDenied);
    if (permissionDenied) {
        Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
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
            errorMessage = formatI18nMessage(
                resolveMessageTemplate(L"Nelson:files_folders_functions:sourceDirectoryDoesExist",
                    L"Source directory %s does not exist or is not a directory."),
                sourceDir.wstring());
            return false;
        }
    }
    if (!FileSystemWrapper::Path::is_directory(sourceDir)) {
        if (!FileSystemWrapper::Path::create_directory(destinationDir)) {
            if (!bForce) {
                errorMessage = formatI18nMessage(
                    resolveMessageTemplate(
                        L"Nelson:files_folders_functions:cannotCreateDestinationDirectory",
                        L"Cannot create destination directory %s"),
                    destinationDir.wstring());
                return false;
            }
        }
    }

    nfs::path rootSrc(sourceDir.wstring());
    nfs::path rootDest(destinationDir.wstring());
    std::error_code iterCode;
    for (const auto& dirEnt : nfs::recursive_directory_iterator(rootSrc, iterCode)) {
        if (iterCode) {
            errorMessage = utf8_to_wstring(iterCode.message());
            return false;
        }
        const nfs::path& entry = dirEnt.path();
        // A relative path keeps a leading separator from resetting the join to the root.
        nfs::path relative = entry.lexically_relative(rootSrc);
        nfs::path destPath = rootDest / relative;
        std::error_code entryCode;
        if (dirEnt.is_directory(entryCode)) {
            nfs::create_directories(destPath, entryCode);
            if (entryCode && !nfs::is_directory(destPath)) {
                errorMessage = utf8_to_wstring(entryCode.message());
                return false;
            }
        } else {
            nfs::path parent = destPath.parent_path();
            if (!parent.empty()) {
                nfs::create_directories(parent, entryCode);
            }
            entryCode.clear();
            nfs::copy_file(entry, destPath, nfs::copy_options::overwrite_existing, entryCode);
            if (entryCode) {
                errorMessage = utf8_to_wstring(entryCode.message());
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
    if (!FileSystemWrapper::Path::is_directory(srcDir, permissionDenied)) {
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        Error(L"Nelson:files_folders_functions:directorySourceDoesNotExist",
            L"Directory source does not exist.");
    }
    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        Error(L"Nelson:files_folders_functions:destinationDoesNotExist",
            L"Directory destination does not exist.");
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
                Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
            }
            Error(L"Nelson:files_folders_functions:cellExistingFilenamesExpected",
                L"A cell of existing filenames expected.");
        }
    }

    if (!FileSystemWrapper::Path::is_directory(destDir, permissionDenied)) {
        if (permissionDenied) {
            Error(L"Nelson:files_folders_functions:permissionDenied", L"Permission denied.");
        }
        Error(L"Nelson:files_folders_functions:destinationDoesNotExist",
            L"Directory destination does not exist.");
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
