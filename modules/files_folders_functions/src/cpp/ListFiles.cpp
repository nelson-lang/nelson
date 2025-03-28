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
#include <filesystem>
#include "ListFiles.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "StringHelpers.hpp"
#include "characters_encoding.hpp"
#include "ParallelSort.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#ifdef _MSC_VER
std::vector<FileInfo>
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    std::vector<FileInfo> res;
    std::filesystem::path path(mask);
    if (std::filesystem::exists(path)) {
        res.push_back(FileInfo(path.wstring()));
    } else {
        std::filesystem::path branch(path.parent_path());
        if (branch.empty()) {
            branch = std::filesystem::current_path();
        }
        if (std::filesystem::is_directory(branch)) {
            std::wstring _mask = path.filename().wstring();
            StringHelpers::replace_all(_mask, L".", L"\\.");
            StringHelpers::replace_all(_mask, L"?", L".");
            StringHelpers::replace_all(_mask, L"*", L".*");
            std::wregex rmask(_mask, std::wregex::icase);
            if (bSubdirectories) {
                bool permissionDenied = false;
                bool isDir = std::filesystem::is_directory(branch.wstring());
                if (isDir) {
                    try {
                        for (std::filesystem::recursive_directory_iterator p(branch.native()), end;
                             p != end; ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current = file;
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
                        std::error_code error_code = e.code();
                        Error(error_code.message());
                    }
                } else {
                    if (permissionDenied) {
                        Error(_W("Permission denied."));
                    }
                }
            } else {
                std::filesystem::path dir = branch;
                if (std::filesystem::is_directory(branch.wstring())) {
                    try {
                        for (std::filesystem::directory_iterator p(branch.native()), end; p != end;
                             ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current(file);
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
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
#else
std::vector<FileInfo>
ListFilesWithWildcard(const std::wstring& _mask, bool bSubdirectories)
{
    std::string mask = wstring_to_utf8(_mask);
    std::vector<FileInfo> res;
    std::filesystem::path path(mask);
    if (std::filesystem::exists(path)) {
        res.push_back(FileInfo(utf8_to_wstring(path.string())));
    } else {
        std::filesystem::path branch(path.parent_path());
        if (branch.empty()) {
            branch = std::filesystem::current_path();
        }
        if (std::filesystem::is_directory(branch)) {
            std::string _mask = path.filename().string();
            StringHelpers::replace_all(_mask, ".", "\\.");
            StringHelpers::replace_all(_mask, "?", ".");
            StringHelpers::replace_all(_mask, "*", ".*");
            std::regex rmask(_mask, std::regex::icase);
            if (bSubdirectories) {
                bool permissionDenied = false;
                bool isDir = std::filesystem::is_directory(branch.string());
                if (isDir) {
                    try {
                        for (std::filesystem::recursive_directory_iterator p(branch.native()), end;
                             p != end; ++p) {
                            if (!std::regex_match(p->path().filename().string(), rmask)) {
                                continue;
                            }
                            std::string file(p->path().string());
                            if (file[0] == '.' && (file[1] == '/' || file[1] == '\\')) {
                                file = std::string(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current = file;
                            res.push_back(FileInfo(utf8_to_wstring(current.string())));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
                        std::error_code error_code = e.code();
                        Error(error_code.message());
                    }
                } else {
                    if (permissionDenied) {
                        Error(_W("Permission denied."));
                    }
                }
            } else {
                std::filesystem::path dir = branch;
                std::filesystem::path r = dir.root_path();
                /*
                if (dir != r)
                {
                    res.push_back(FileInfo(branch.wstring() + L"/.", bSubdirectories));
                    res.push_back(FileInfo(branch.wstring() + L"/..", bSubdirectories));
                }
                */
                if (std::filesystem::is_directory(branch.string())) {
                    try {
                        for (std::filesystem::directory_iterator p(branch.native()), end; p != end;
                             ++p) {
                            if (!std::regex_match(p->path().filename().string(), rmask)) {
                                continue;
                            }
                            std::string file(p->path().string());
                            if (file[0] == '.' && (file[1] == '/' || file[1] == '\\')) {
                                file = std::string(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current(file);
                            res.push_back(FileInfo(utf8_to_wstring(current.string())));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
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
#endif
//=============================================================================
#ifdef _MSC_VER
std::vector<FileInfo>
ListFiles(const std::wstring& directory, bool bSubdirectories)
{
    std::vector<FileInfo> res;
    std::size_t foundstar = directory.find_first_of(L'*');
    std::size_t foundInterrogationMark = directory.find_first_of(L'?');
    bool bWithWildCard
        = (foundstar != std::wstring::npos) || (foundInterrogationMark != std::wstring::npos);
    if (bWithWildCard || directory == L".") {
        if (directory == L".") {
            res = ListFilesWithWildcard(L"*", bSubdirectories);
        } else {
            res = ListFilesWithWildcard(directory, bSubdirectories);
        }
    } else {
        if (std::filesystem::is_regular_file(directory)) {
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
            std::filesystem::path thispath = directorymodified;
            std::filesystem::path branch(thispath.parent_path());
            if (branch.empty()) {
                branch = std::filesystem::current_path() / directory;
            } else {
                if (branch.generic_wstring().back() == L':') {
                    branch = branch.generic_wstring() + L"/";
                }
            }
            if (!std::filesystem::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (std::filesystem::is_directory(branch.wstring())) {
                    try {
                        for (std::filesystem::recursive_directory_iterator
                                 dir_iter(branch.native()),
                             end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            std::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            } else {
                std::filesystem::path dir = branch;
                std::filesystem::path r = dir.root_path();
                if (std::filesystem::is_directory(directory)) {
                    if (dir != r) {
                        res.push_back(FileInfo(directory + L"/."));
                        res.push_back(FileInfo(directory + L"/.."));
                    }
                    try {
                        for (std::filesystem::directory_iterator dir_iter(directory), end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring()));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
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
            parallelSort(res.begin(), res.end(), customIsDirLess);
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
            parallelSort(res.begin(), res.end(), customFilenameLess);
        }
    }
    return res;
}
#else
std::vector<FileInfo>
ListFiles(const std::wstring& _directory, bool bSubdirectories)
{
    std::string directory = wstring_to_utf8(_directory);
    std::vector<FileInfo> res;
    std::size_t foundstar = directory.find_first_of('*');
    std::size_t foundInterrogationMark = directory.find_first_of('?');
    bool bWithWildCard
        = (foundstar != std::string::npos) || (foundInterrogationMark != std::string::npos);
    if (bWithWildCard || directory == ".") {
        if (directory == ".") {
            res = ListFilesWithWildcard(L"*", bSubdirectories);
        } else {
            res = ListFilesWithWildcard(_directory, bSubdirectories);
        }
    } else {
        if (std::filesystem::is_regular_file(directory)) {
            res.push_back(FileInfo(_directory));
        } else {
            if (_directory.empty()) {
                res.clear();
                return res;
            }
            std::string directorymodified;
            if (StringHelpers::ends_with(directory, "/")
                || StringHelpers::ends_with(directory, "\\")) {
                directorymodified = directory;
            } else {
                directorymodified = directory + "/";
            }
            std::filesystem::path thispath = directorymodified;
            std::filesystem::path branch(thispath.parent_path());
            if (branch.empty()) {
                branch = std::filesystem::current_path() / directory;
            } else {
                if (branch.generic_string().back() == ':') {
                    branch = branch.generic_string() + "/";
                }
            }
            if (!std::filesystem::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (std::filesystem::is_directory(branch.string())) {
                    try {
                        for (std::filesystem::recursive_directory_iterator
                                 dir_iter(branch.native()),
                             end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(utf8_to_wstring(current.string())));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            std::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            } else {
                std::filesystem::path dir = branch;
                std::filesystem::path r = dir.root_path();
                if (std::filesystem::is_directory(directory)) {
                    if (dir != r) {
                        res.push_back(FileInfo(utf8_to_wstring(directory + "/.")));
                        res.push_back(FileInfo(utf8_to_wstring(directory + "/..")));
                    }
                    try {
                        for (std::filesystem::directory_iterator dir_iter(directory), end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(utf8_to_wstring(current.string())));
                        }
                    } catch (const std::filesystem::filesystem_error& e) {
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
            parallelSort(res.begin(), res.end(), customIsDirLess);
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
            parallelSort(res.begin(), res.end(), customFilenameLess);
        }
    }
    return res;
}
#endif
//=============================================================================
} // namespace Nelson
//=============================================================================
