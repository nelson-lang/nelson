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
#include "PredefinedErrorMessages.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& filenames)
{
    if (!FileSystemWrapper::Path::is_directory(rootpath)) {
        FileSystemWrapper::Path p(rootpath);
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(p, errorMessage)) {
            raiseError(L"Nelson:file_archiver:ERROR_CANNOT_CREATE_DIRECTORY",
                ERROR_CANNOT_CREATE_DIRECTORY);
        }
    }
    std::wstring fullRootPath = getRootPath(rootpath);

    unzFile* zipfile = static_cast<unzFile*>(unzOpen64(wstring_to_utf8(zipFilename).c_str()));
    if (zipfile == nullptr) {
        raiseError(
            L"Nelson:file_archiver:ERROR_CANNOT_READ_FILE", ERROR_CANNOT_READ_FILE, zipFilename);
    }

    unz_global_info64 global_info;
    if (unzGetGlobalInfo64(zipfile, &global_info) != UNZ_OK) {
        raiseError(L"Nelson:file_archiver:ERROR_CANNOT_READ_FILE_GLOBAL_INFO",
            ERROR_CANNOT_READ_FILE_GLOBAL_INFO);
    }
#define MAX_FILENAME 8192
    for (size_t i = 0; i < global_info.number_entry; ++i) {
        unz_file_info64 file_info;
        char filename[MAX_FILENAME];
        if (unzGetCurrentFileInfo64(
                zipfile, &file_info, filename, MAX_FILENAME, nullptr, 0, nullptr, 0)
            != UNZ_OK) {
            unzClose(zipfile);
            raiseError(
                L"Nelson:file_archiver:ERROR_CANNOT_READ_FILE_INFO", ERROR_CANNOT_READ_FILE_INFO);
        }
        const size_t filename_length = strlen(filename);
        if (filename[filename_length - 1] == '/') {
            std::wstring completePath = fullRootPath + L"/" + utf8_to_wstring(filename);
            if (!FileSystemWrapper::Path::is_directory(completePath)) {
                FileSystemWrapper::Path::create_directories(completePath);
            }
            filenames.push_back(completePath);
        } else {
            if (unzOpenCurrentFile(zipfile) != UNZ_OK) {
                unzClose(zipfile);
                raiseError(L"Nelson:file_archiver:ERROR_CANNOT_OPEN_FILE", ERROR_CANNOT_OPEN_FILE);
            }
            std::filesystem::path completePath(fullRootPath + L"/" + utf8_to_wstring(filename));
            std::error_code ec;
            std::filesystem::create_directories(completePath.parent_path(), ec);
            if (ec) {
                unzCloseCurrentFile(zipfile);
                unzClose(zipfile);
                raiseError(L"Nelson:file_archiver:ERROR_CANNOT_CREATE_INTERMEDIATE_DIRECTORY",
                    ERROR_CANNOT_CREATE_INTERMEDIATE_DIRECTORY);
            }

#ifdef _MSC_VER
            FILE* out = _wfopen(completePath.generic_wstring().c_str(), L"wb");
#else
            FILE* out = fopen(wstring_to_utf8(completePath.generic_wstring()).c_str(), "wb");
#endif
            if (out == nullptr) {
                unzCloseCurrentFile(zipfile);
                unzClose(zipfile);
                raiseError(L"Nelson:file_archiver:ERROR_CANNOT_OPEN_DESTINATION_FILE",
                    ERROR_CANNOT_OPEN_DESTINATION_FILE);
            }
            filenames.push_back(completePath.generic_wstring());

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
                    raiseError(
                        L"Nelson:file_archiver:ERROR_CANNOT_READ_DATA", ERROR_CANNOT_READ_DATA);
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
                raiseError(L"Nelson:file_archiver:ERROR_CANNOT_READ_NEXT_FILE",
                    ERROR_CANNOT_READ_NEXT_FILE);
            }
        }
    }
    unzClose(zipfile);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
