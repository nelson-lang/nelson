//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <filesystem>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include "ListFiles.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "IsFile.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
boost::container::vector<FileInfo>
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    boost::container::vector<FileInfo> res;
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
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\."), L"\\\\.");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\?"), L".");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\*"), L".*");
            boost::wregex rmask(_mask, boost::wregex::icase);
            if (bSubdirectories) {
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (std::filesystem::recursive_directory_iterator p(branch), end; p != end;
                             ++p) {
                            if (!boost::regex_match(p->path().filename().wstring(), rmask)) {
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
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                            if (!boost::regex_match(p->path().filename().wstring(), rmask)) {
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
boost::container::vector<FileInfo>
ListFiles(const std::wstring& directory, bool bSubdirectories)
{
    boost::container::vector<FileInfo> res;
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
        if (IsFile(directory)) {
            res.push_back(FileInfo(directory));
        } else {
            if (directory.empty()) {
                res.clear();
                return res;
            }
            std::wstring directorymodified;
            if (boost::algorithm::ends_with(directory, L"/")
                || boost::algorithm::ends_with(directory, L"\\")) {
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
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (std::filesystem::recursive_directory_iterator dir_iter(branch), end;
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
                if (IsDirectory(directory)) {
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
