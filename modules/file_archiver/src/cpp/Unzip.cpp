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
void
UnZip(const std::wstring& zipFilename, const std::wstring& rootpath, wstringVector& filenames)
{
    if (!FileSystemWrapper::Path::is_directory(rootpath)) {
        FileSystemWrapper::Path p(rootpath);
        std::string errorMessage;
        if (!FileSystemWrapper::Path::create_directories(p, errorMessage)) {
            Error(_W("Cannot create directory."));
        }
    }
    std::wstring fullRootPath = getRootPath(rootpath);

    unzFile* zipfile = static_cast<unzFile*>(unzOpen64(wstring_to_utf8(zipFilename).c_str()));
    if (zipfile == nullptr) {
        Error(_W("Cannot read file:") + L" " + zipFilename);
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
            std::wstring completePath = fullRootPath + L"/" + utf8_to_wstring(filename);
            if (!FileSystemWrapper::Path::is_directory(completePath)) {
                FileSystemWrapper::Path::create_directories(completePath);
            }
            filenames.push_back(completePath);
        } else {
            if (unzOpenCurrentFile(zipfile) != UNZ_OK) {
                unzClose(zipfile);
                Error(_W("Cannot open file."));
            }
            std::wstring completePath = fullRootPath + L"/" + utf8_to_wstring(filename);
#ifdef _MSC_VER
            FILE* out = _wfopen(completePath.c_str(), L"wb");
#else
            FILE* out = fopen(wstring_to_utf8(completePath).c_str(), "wb");
#endif
            if (out == nullptr) {
                unzCloseCurrentFile(zipfile);
                unzClose(zipfile);
                Error(_W("Cannot open destination file."));
            }
            filenames.push_back(completePath);

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
            changeFileDate(completePath, file_info.tmu_date, file_info.dosDate);
            changeFileOrFolderAttributes(completePath, file_info.external_fa);
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
