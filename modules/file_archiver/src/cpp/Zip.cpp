//=============================================================================
// Copyright (c) 2016present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <boost/filesystem/operations.hpp>
#include <mz_os.h>
#include "Zip.hpp"
#include "Zipper.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static wstringVector
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    wstringVector res;
    boost::filesystem::path path(mask);
    if (boost::filesystem::exists(path)) {
        res.push_back(path.generic_path().wstring());

    } else {
        boost::filesystem::path branch(path.branch_path());
        if (branch.empty()) {
            branch = boost::filesystem::current_path();
        }
        if (boost::filesystem::is_directory(branch)) {
            std::wstring _mask = path.leaf().wstring();

            _mask = boost::regex_replace(_mask, boost::wregex(L"\\."), L"\\\\.");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\?"), L".");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\*"), L".*");

            boost::wregex rmask(_mask, boost::wregex::icase);
            if (bSubdirectories) {
                if (isExistingDirectory(branch.wstring())) {
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
                                res.push_back(current.generic_path().wstring());
                            }
                        }

                    } catch (const boost::filesystem::filesystem_error&) {
                    }
                }

            } else {
                boost::filesystem::path dir = branch;
                boost::filesystem::path r = dir.root_path();
                if (isExistingDirectory(branch.wstring())) {
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
                                res.push_back(current.generic_path().wstring());
                            }
                        }

                    } catch (const boost::filesystem::filesystem_error&) {
                    }
                }
            }
        }
    }
    return res;
}
//=============================================================================
static wstringVector
ListFiles(const std::wstring& directory, bool bSubdirectories)
{
    wstringVector res;
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
        if (isExistingFile(directory)) {
            boost::filesystem::path d = directory;
            res.push_back(d.generic_path().generic_wstring());

        } else {
            if (directory.empty()) {
                res.clear();
                return res;
            }
            boost::filesystem::path thispath = directory;
            thispath = thispath.generic_path();
            if (!boost::algorithm::ends_with(thispath.generic_wstring(), L"/")) {
                thispath = thispath.generic_wstring() + L"/";
            }
            boost::filesystem::path branch(thispath.branch_path());
            if (branch.empty()) {
                branch = boost::filesystem::current_path() / directory;

            } else {
                if (branch.generic_wstring().back() == L':') {
                    branch = branch.generic_path().generic_wstring() + L"/";
                }
            }
            if (!boost::filesystem::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (isExistingDirectory(branch.wstring())) {
                    try {
                        for (boost::filesystem::recursive_directory_iterator dir_iter(branch), end;
                             dir_iter != end; ++dir_iter) {
                            boost::filesystem::path current = dir_iter->path();
                            res.push_back(current.generic_path().wstring());
                        }

                    } catch (const boost::filesystem::filesystem_error&) {
                    }
                }
            } else {
                boost::filesystem::path dir = branch;
                boost::filesystem::path r = dir.root_path();
                if (isExistingDirectory(directory)) {
                    try {
                        for (boost::filesystem::directory_iterator dir_iter(directory), end;
                             dir_iter != end; ++dir_iter) {
                            boost::filesystem::path current = dir_iter->path();
                            res.push_back(current.generic_path().wstring());
                        }

                    } catch (const boost::filesystem::filesystem_error&) {
                    }
                }
            }
        }
    }
    if (!bSubdirectories) {
        if (!res.empty()) {
            std::sort(res.begin(), res.end());
        }
    }
    return res;
}
//=============================================================================
static void
prepareFilesToZip(const wstringVector& names, const std::wstring& rootpath,
    wstringVector& localFiles, wstringVector& filesInZip)

{
    wstringVector filteredNames = names;
    std::sort(filteredNames.begin(), filteredNames.end());
    filteredNames.erase(
        std::unique(filteredNames.begin(), filteredNames.end()), filteredNames.end());

    wstringVector filenames;
    wstringVector paths;
    boost::filesystem::path rootPath = getRootPath(rootpath);

    for (std::wstring f : filteredNames) {
        boost::filesystem::path p = f;
        if (p.is_absolute()) {
            filenames.emplace_back(normalizePath(p.filename().generic_wstring()));
            paths.emplace_back(normalizePath(p.parent_path().generic_path().generic_wstring()));

        } else {
            boost::filesystem::path rp = rootpath;
            paths.emplace_back(normalizePath(rp.generic_path().generic_wstring()));
            boost::filesystem::path fp = f;
            filenames.emplace_back(normalizePath(fp.generic_path().generic_wstring()));
        }
    }
    for (size_t k = 0; k < filenames.size(); ++k) {
        boost::filesystem::path fullname;
        boost::filesystem::path path = paths[k];
        boost::filesystem::path file = filenames[k];
        if (path == L".") {
            fullname = boost::filesystem::current_path() / file;
        } else {
            fullname = path / file;
        }
        boost::filesystem::path genericPath = fullname.generic_path();
        bool isDir = isExistingDirectory(genericPath.generic_wstring());
        bool hasStar = boost::algorithm::contains(genericPath.generic_wstring(), "*")
            || boost::algorithm::contains(genericPath.generic_wstring(), "?");
        wstringVector res;
        if (hasStar) {
            res = ListFiles(genericPath.generic_wstring(), false);
        } else if (isDir) {
            res = ListFiles(genericPath.generic_wstring(), true);
        } else if (isExistingFile(fullname.generic_wstring())) {
            res.emplace_back(fullname.generic_wstring());
        } else {
            Error(_W("Invalid value."));
        }
        std::wstring pathwstr;
        if (rootpath == L"." && path != L".") {
            pathwstr = path.generic_wstring();
        } else {
            pathwstr = rootPath.generic_wstring();
        }
        for (size_t j = 0; j < res.size(); ++j) {
            std::wstring name = res[j];
            localFiles.emplace_back(normalizePath(name));
            std::wstring entry;
            if (boost::algorithm::starts_with(name, pathwstr)) {
                entry = name.substr(pathwstr.size());
            }
            filesInZip.emplace_back(normalizePath(entry));
        }
    }
}
//=============================================================================
void
Zip(const std::wstring& zipFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames)

{
    if (!rootpath.empty() && !isExistingDirectory(rootpath)) {
        Error(_W("Invalid root path."));
    }
    boost::filesystem::path p = rootpath;
    wstringVector localFiles;
    wstringVector filesInZip;
    prepareFilesToZip(names, rootpath, localFiles, filesInZip);
    if (localFiles.empty()) {
        Error(_W("Nothing to zip."));
    }
    Nelson::Zipper zipFile;
    if (isExistingFile(zipFilename)) {
        boost::filesystem::path pathToRemove = zipFilename;
        boost::filesystem::remove(pathToRemove);
    }
    zipFile.open(wstring_to_utf8(zipFilename).c_str(), false);
    if (!zipFile.isOpen()) {
        Error(_W("Cannot write file:") + L" " + zipFilename);
    }
    for (size_t k = 0; k < localFiles.size(); ++k) {
        std::wstring filename = localFiles[k];
        std::wstring entry = filesInZip[k];
        if (isExistingDirectory(filename)) {
            uint32_t attributes;
            mz_os_get_file_attribs(wstring_to_utf8(filename).c_str(), &attributes);
            if (!boost::algorithm::ends_with(entry, L"/")) {
                entry = entry + L"/";
            }
            if (boost::algorithm::starts_with(entry, L"/")) {
                entry = entry.substr(1);
            }
            zipFile.addEntry(wstring_to_utf8(entry).c_str(), attributes);
            zipFile.closeEntry();
            entrynames.push_back(entry);

        } else {
#ifdef _MSC_VER
            std::ifstream file(filename, std::ios::in | std::ios::binary);
#else
            std::ifstream file(wstring_to_utf8(filename), std::ios::in | std::ios::binary);
#endif
            if (!file.is_open()) {
                zipFile.close();
                Error(_W("Cannot read file:") + L" " + filename);
            }
            uint32_t attributes;
            mz_os_get_file_attribs(wstring_to_utf8(filename).c_str(), &attributes);
            if (boost::algorithm::starts_with(entry, L"/")) {
                entry = entry.substr(1);
            }
            if (zipFile.addEntry(wstring_to_utf8(entry).c_str(), attributes)) {
                zipFile << file;
                file.close();
                zipFile.closeEntry();
                entrynames.push_back(entry);
            } else {
                zipFile.close();
                Error(_W("Cannot add entry:") + L" " + entry);
            }
        }
    }
    zipFile.close();
    std::sort(entrynames.begin(), entrynames.end());
    entrynames.erase(std::unique(entrynames.begin(), entrynames.end()), entrynames.end());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
