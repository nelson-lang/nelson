//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
#include <mz_compat.h>
#include "FileSystemWrapper.hpp"
#include "Unzip.hpp"
#include "UnzipHelpers.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
withZipExtensionForRead(const std::wstring& zipFilename)
{
    FileSystemWrapper::Path p(zipFilename);
    if (p.extension().wstring().empty()) {
        return zipFilename + L".zip";
    }
    return zipFilename;
}
//=============================================================================
static std::wstring
getReturnedFilename(const std::wstring& rootpath, const std::wstring& fullRootPath,
    const std::wstring& entryName, bool isDirectory)
{
    FileSystemWrapper::Path root(rootpath.empty() ? L"." : rootpath);
    FileSystemWrapper::Path result;
    if (root.is_absolute()) {
        result = FileSystemWrapper::Path(fullRootPath) / FileSystemWrapper::Path(entryName);
    } else if (rootpath.empty() || rootpath == L".") {
        result = FileSystemWrapper::Path(entryName);
    } else {
        result = root / FileSystemWrapper::Path(entryName);
    }
    std::wstring returnedName = normalizeZipPath(result.generic_wstring());
    if (isDirectory && !StringHelpers::ends_with(returnedName, L"/")) {
        returnedName = returnedName + L"/";
    }
    return returnedName;
}
//=============================================================================
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& filenames)
{
    UnZip(zipFilename, rootpath, L"", filenames);
}
//=============================================================================
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, const std::wstring& password,
    wstringVector& filenames)
{
    if (!FileSystemWrapper::Path::is_directory(rootpath)) {
        FileSystemWrapper::Path p(rootpath);
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(p, errorMessage)) {
            Error(_W("Cannot create directory."));
        }
    }
    std::wstring fullRootPath = getRootPath(rootpath);
    std::wstring zipFilenameWithExtension = withZipExtensionForRead(zipFilename);
    std::string utf8Password;
    const char* passwordPtr = nullptr;
    if (!password.empty()) {
        utf8Password = wstring_to_utf8(password);
        passwordPtr = utf8Password.c_str();
    }

    unzFile* zipfile
        = static_cast<unzFile*>(unzOpen64(wstring_to_utf8(zipFilenameWithExtension).c_str()));
    if (zipfile == nullptr) {
        Error(_W("Cannot read file:") + L" " + zipFilenameWithExtension);
    }

    unz_global_info64 global_info;
    if (unzGetGlobalInfo64(zipfile, &global_info) != UNZ_OK) {
        Error(_W("Cannot read file global info."));
    }
#define MAX_FILENAME 8192
    for (size_t i = 0; i < global_info.number_entry; ++i) {
        unz_file_info64 file_info;
        char filename[MAX_FILENAME];
        if (unzGetCurrentFileInfo64(
                zipfile, &file_info, filename, MAX_FILENAME, nullptr, 0, nullptr, 0)
            != UNZ_OK) {
            unzClose(zipfile);
            Error(_W("Cannot read file info."));
        }
        const size_t filename_length = strlen(filename);
        if (filename[filename_length - 1] == '/') {
            std::wstring entryName = utf8_to_wstring(filename);
            std::wstring completePath = fullRootPath + L"/" + entryName;
            if (!FileSystemWrapper::Path::is_directory(completePath)) {
                FileSystemWrapper::Path::create_directories(completePath);
            }
            filenames.push_back(getReturnedFilename(rootpath, fullRootPath, entryName, true));
        } else {
            int openStatus = passwordPtr == nullptr
                ? unzOpenCurrentFile(zipfile)
                : unzOpenCurrentFilePassword(zipfile, passwordPtr);
            if (openStatus != UNZ_OK) {
                unzClose(zipfile);
                Error(_W("Cannot open file."));
            }
            std::wstring entryName = utf8_to_wstring(filename);
            std::filesystem::path completePath(fullRootPath + L"/" + entryName);
            std::error_code ec;
            std::filesystem::create_directories(completePath.parent_path(), ec);
            if (ec) {
                unzCloseCurrentFile(zipfile);
                unzClose(zipfile);
                Error(_W("Cannot create intermediate directory."));
            }

#ifdef _MSC_VER
            FILE* out = _wfopen(completePath.generic_wstring().c_str(), L"wb");
#else
            FILE* out = fopen(wstring_to_utf8(completePath.generic_wstring()).c_str(), "wb");
#endif
            if (out == nullptr) {
                unzCloseCurrentFile(zipfile);
                unzClose(zipfile);
                Error(_W("Cannot open destination file."));
            }
            filenames.push_back(getReturnedFilename(rootpath, fullRootPath, entryName, false));

            int error = UNZ_OK;
            char read_buffer[MAX_FILENAME];
            do {
                error = unzReadCurrentFile(zipfile, read_buffer, MAX_FILENAME);
                if (error < 0) {
                    unzCloseCurrentFile(zipfile);
                    unzClose(zipfile);
                    if (out != nullptr) {
                        fclose(out);
                        out = nullptr;
                    }
                    Error(_W("Cannot read data."));
                }
                if (error > 0) {
                    fwrite(read_buffer, error, 1, out);
                }
            } while (error > 0);
            unzCloseCurrentFile(zipfile);
            if (out != nullptr) {
                fclose(out);
                out = nullptr;
            }
            changeFileDate(completePath.generic_wstring(), file_info.tmu_date, file_info.dosDate);
            changeFileOrFolderAttributes(completePath.generic_wstring(), file_info.external_fa);
        }
        if ((i + 1) < global_info.number_entry) {
            if (unzGoToNextFile(zipfile) != UNZ_OK) {
                unzClose(zipfile);
                Error(_W("Cannot read next file."));
            }
        }
    }
    unzClose(zipfile);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
