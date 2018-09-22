//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "ListFiles.hpp"
#include "Error.hpp"
#include "IsDirectory.hpp"
#include "IsFile.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/regex.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
boost::container::vector<FileInfo>
ListFilesWithWildcard(std::wstring mask, bool bSubdirectories)
{
    boost::container::vector<FileInfo> res;
    boost::filesystem::path path(mask);
    if (boost::filesystem::exists(path)) {
        res.push_back(FileInfo(path.wstring()));
    } else {
        boost::filesystem::path branch(path.branch_path());
        if (branch.empty()) {
            branch = boost::filesystem::current_path();
        }
        if (boost::filesystem::is_directory(branch)) {
            mask = path.leaf().wstring();
            static const std::pair<boost::wregex, const wchar_t*> repl[] = {
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\."), L"\\\\."),
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\?"), L"."),
                std::pair<boost::wregex, const wchar_t*>(boost::wregex(L"\\*"), L".*"),
            };
            for (const std::pair<boost::wregex, const wchar_t*>* r = repl;
                 r < repl + sizeof(repl) / sizeof(*repl); ++r) {
                mask = boost::regex_replace(mask, r->first, r->second);
            }
            boost::wregex rmask(mask, boost::wregex::icase);
            if (bSubdirectories) {
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (boost::filesystem::recursive_directory_iterator p(branch), end;
                             p != end; ++p) {
                            if (!boost::regex_match(p->path().leaf().wstring(), rmask)) {
                                continue;
                            } else {
                                std::wstring file(p->path().wstring());
                                if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                    file = std::wstring(file.begin() + 2, file.end());
                                }
                                boost::filesystem::path current = file;
                                res.push_back(FileInfo(current.wstring(), bSubdirectories));
                            }
                        }
                    } catch (const boost::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            boost::system::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            } else {
                boost::filesystem::path dir = branch;
                boost::filesystem::path r = dir.root_path();
                /*
                if (dir != r)
                {
                    res.push_back(FileInfo(branch.wstring() + L"/.", bSubdirectories));
                    res.push_back(FileInfo(branch.wstring() + L"/..", bSubdirectories));
                }
                */
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (boost::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                            if (!boost::regex_match(p->path().leaf().wstring(), rmask)) {
                                continue;
                            } else {
                                std::wstring file(p->path().wstring());
                                if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                    file = std::wstring(file.begin() + 2, file.end());
                                }
                                boost::filesystem::path current = file;
                                res.push_back(FileInfo(current.wstring(), bSubdirectories));
                            }
                        }
                    } catch (const boost::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            boost::system::error_code error_code = e.code();
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
    std::size_t foundstar = directory.find_first_of(L"*");
    std::size_t foundInterrogationMark = directory.find_first_of(L"?");
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
            boost::filesystem::path thispath = directorymodified;
            boost::filesystem::path branch(thispath.branch_path());
            if (branch.empty()) {
                branch = boost::filesystem::current_path() / directory;
            } else {
                if (branch.generic_wstring().back() == L':') {
                    branch = branch.generic_wstring() + L"/";
                }
            }
            if (!boost::filesystem::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (IsDirectory(branch.wstring())) {
                    try {
                        for (boost::filesystem::recursive_directory_iterator dir_iter(branch), end;
                             dir_iter != end; ++dir_iter) {
                            boost::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring(), bSubdirectories));
                        }
                    } catch (const boost::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            boost::system::error_code error_code = e.code();
                            Error(error_code.message());
                        }
                    }
                }
            } else {
                boost::filesystem::path dir = branch;
                boost::filesystem::path r = dir.root_path();
                if (IsDirectory(directory)) {
                    if (dir != r) {
                        res.push_back(FileInfo(directory + L"/.", bSubdirectories));
                        res.push_back(FileInfo(directory + L"/..", bSubdirectories));
                    }
                    try {
                        for (boost::filesystem::directory_iterator dir_iter(directory), end;
                             dir_iter != end; ++dir_iter) {
                            boost::filesystem::path current = dir_iter->path();
                            res.push_back(FileInfo(current.wstring(), bSubdirectories));
                        }
                    } catch (const boost::filesystem::filesystem_error& e) {
                        if (!bSubdirectories) {
                            boost::system::error_code error_code = e.code();
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
            operator()(FileInfo a, FileInfo b) // lgtm [cpp/large-parameter]
            {
                return a.getName() < b.getName();
            }
        } customFilenameLess;
        std::sort(res.begin(), res.end(), customFilenameLess);
        struct
        {
            bool
            operator()(FileInfo a, FileInfo b) // lgtm [cpp/large-parameter]
            {
                return a.isDir() > b.isDir();
            }
        } customIsDirLess;
        std::sort(res.begin(), res.end(), customIsDirLess);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
