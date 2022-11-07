//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <regex>
#include "ListFiles.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::vector<FileInfo>
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    std::vector<FileInfo> res;
    nfs::path path(mask);
    if (nfs::exists(path)) {
        res.push_back(FileInfo(path.wstring()));
    } else {
        nfs::path branch(path.parent_path());
        if (branch.empty()) {
            branch = nfs::current_path();
        }
        if (nfs::is_directory(branch)) {
            std::wstring _mask = path.filename().wstring();
            StringHelpers::replace_all(_mask, L".", L"\\.");
            StringHelpers::replace_all(_mask, L"?", L".");
            StringHelpers::replace_all(_mask, L"*", L".*");
            std::wregex rmask(_mask, std::wregex::icase);
            if (bSubdirectories) {
                bool permissionDenied = false;
                bool isDir
                    = FileSystemWrapper::Path::is_directory(branch.wstring(), permissionDenied);
                if (isDir) {
                    try {
                        for (nfs::recursive_directory_iterator p(branch.native()), end; p != end;
                             ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            nfs::path current = file;
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const nfs::filesystem_error& e) {
                        std::error_code error_code = e.code();
                        Error(error_code.message());
                    }
                } else {
                    if (permissionDenied) {
                        Error(_W("Permission denied."));
                    }
                }
            } else {
                nfs::path dir = branch;
                nfs::path r = dir.root_path();
                /*
                if (dir != r)
                {
                    res.push_back(FileInfo(branch.wstring() + L"/.", bSubdirectories));
                    res.push_back(FileInfo(branch.wstring() + L"/..", bSubdirectories));
                }
                */
                if (FileSystemWrapper::Path::is_directory(branch.wstring())) {
                    try {
                        for (nfs::directory_iterator p(branch.native()), end; p != end; ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            nfs::path current(file);
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const nfs::filesystem_error& e) {
                        if (!bSubdirectories) {
                            std::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
std::vector<FileInfo>
ListFiles(const std::wstring& directory, bool bSubdirectories)
{
    std::vector<FileInfo> res;
    std::size_t foundstar = directory.find_first_of(L'*');
    std::size_t foundInterrogationMark = directory.find_first_of(L'?');
    bool bWithWildCard
        = (foundstar != std::string::npos) || (foundInterrogationMark != std::string::npos);
    if (bWithWildCard || directory == L".") {
        if (directory == L".") {
            res = ListFilesWithWildcard(L"*", bSubdirectories);
        } else {
            res = ListFilesWithWildcard(directory, bSubdirectories);
        }
    } else {
        if (FileSystemWrapper::Path::is_regular_file(directory)) {
            res.push_back(FileInfo(directory));
        } else {
            if (directory.empty()) {
                res.clear();
                return res;
            }
            std::wstring directorymodified;
            if (StringHelpers::ends_with(directory, L"/")
                || StringHelpers::ends_with(directory, L"\\")) {
                directorymodified = directory;
            } else {
                directorymodified = directory + L"/";
            }
            nfs::path thispath = directorymodified;
            nfs::path branch(thispath.parent_path());
            if (branch.empty()) {
                branch = nfs::current_path() / directory;
            } else {
                if (branch.generic_wstring().back() == L':') {
                    branch = branch.generic_wstring() + L"/";
                }
            }
            if (!nfs::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (FileSystemWrapper::Path::is_directory(branch.wstring())) {
                    try {
                        for (nfs::recursive_directory_iterator dir_iter(branch.native()), end;
                             dir_iter != end; ++dir_iter) {
                            nfs::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const nfs::filesystem_error& e) {
                        if (!bSubdirectories) {
                            std::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            } else {
                nfs::path dir = branch;
                nfs::path r = dir.root_path();
                if (FileSystemWrapper::Path::is_directory(directory)) {
                    if (dir != r) {
                        res.push_back(FileInfo(directory + L"/."));
                        res.push_back(FileInfo(directory + L"/.."));
                    }
                    try {
                        for (nfs::directory_iterator dir_iter(directory), end; dir_iter != end;
                             ++dir_iter) {
                            nfs::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const nfs::filesystem_error& e) {
                        if (!bSubdirectories) {
                            std::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            }
        }
    }
    if (!res.empty() && !bSubdirectories) {
        struct
        {
            bool
            operator()(FileInfo a, FileInfo b)
            {
                return static_cast<int>(a.isDir()) > static_cast<int>(b.isDir());
            }
        } customIsDirLess;
        if (!res.empty()) {
            std::sort(res.begin(), res.end(), customIsDirLess);
        }
        struct
        {
            bool
            operator()(FileInfo a, FileInfo b)
            {
                return a.getName() < b.getName();
            }
        } customFilenameLess;
        if (!res.empty()) {
            std::sort(res.begin(), res.end(), customFilenameLess);
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
