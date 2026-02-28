//=============================================================================
// Copyright (c) 2016 - present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <fstream>
#include <regex>
#include <mz_os.h>
#include "Zip.hpp"
#include "Zipper.hpp"
#include "ZipHelpers.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static wstringVector
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    wstringVector res;
    nfs::path path(mask);
    if (nfs::exists(path)) {
        res.push_back(path.generic_wstring());

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
                if (FileSystemWrapper::Path::is_directory(branch.wstring())) {
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
                            FileSystemWrapper::Path current = file;
                            res.push_back(current.generic_path().wstring());
                        }

                    } catch (const nfs::filesystem_error&) {
                    }
                }

            } else {
                FileSystemWrapper::Path dir(branch.wstring());
                if (FileSystemWrapper::Path::is_directory(branch.wstring())) {
                    try {
                        for (nfs::directory_iterator p(branch), end; p != end; ++p) {
                            if (!std::regex_match(p->path().filename().wstring(), rmask)) {
                                continue;
                            }
                            std::wstring file(p->path().wstring());
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            FileSystemWrapper::Path current = file;
                            res.push_back(current.generic_path().wstring());
                        }

                    } catch (const nfs::filesystem_error&) {
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
        if (FileSystemWrapper::Path::is_regular_file(directory)) {
            nfs::path d = directory;
            res.push_back(d.generic_wstring());

        } else {
            if (directory.empty()) {
                res.clear();
                return res;
            }
            nfs::path thispath = directory;
            thispath = thispath.generic_wstring();
            if (!StringHelpers::ends_with(thispath.generic_wstring(), L"/")) {
                thispath = thispath.generic_wstring() + L"/";
            }
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
                            res.push_back(current.generic_wstring());
                        }

                    } catch (const nfs::filesystem_error&) {
                    }
                }
            } else {
                if (FileSystemWrapper::Path::is_directory(directory)) {
                    try {
                        for (nfs::directory_iterator dir_iter(directory), end; dir_iter != end;
                             ++dir_iter) {
                            nfs::path current = dir_iter->path();
                            res.push_back(current.generic_wstring());
                        }

                    } catch (const nfs::filesystem_error&) {
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
    FileSystemWrapper::Path rootPath = getRootPath(rootpath);

    for (const std::wstring& f : filteredNames) {
        FileSystemWrapper::Path p = f;
        if (p.is_absolute()) {
            filenames.emplace_back(normalizeZipPath(p.filename().generic_wstring()));
            paths.emplace_back(normalizeZipPath(p.parent_path().generic_path().generic_wstring()));

        } else {
            FileSystemWrapper::Path rp = rootpath;
            paths.emplace_back(normalizeZipPath(rp.generic_path().generic_wstring()));
            FileSystemWrapper::Path fp = f;
            filenames.emplace_back(normalizeZipPath(fp.generic_path().generic_wstring()));
        }
    }
    for (size_t k = 0; k < filenames.size(); ++k) {
        FileSystemWrapper::Path fullname;
        FileSystemWrapper::Path path = paths[k];
        FileSystemWrapper::Path file = filenames[k];
        if (path.wstring() == L".") {
            fullname = FileSystemWrapper::Path::current_path() / file;
        } else {
            fullname = path / file;
        }
        FileSystemWrapper::Path genericPath = fullname.generic_path();
        bool isDir = FileSystemWrapper::Path::is_directory(genericPath.generic_wstring());
        bool hasStar = StringHelpers::contains(genericPath.generic_wstring(), L"*")
            || StringHelpers::contains(genericPath.generic_wstring(), L"?");
        wstringVector res;
        if (hasStar) {
            res = ListFiles(genericPath.generic_wstring(), false);
        } else if (isDir) {
            res = ListFiles(genericPath.generic_wstring(), true);
        } else if (FileSystemWrapper::Path::is_regular_file(fullname.generic_wstring())) {
            res.emplace_back(fullname.generic_wstring());
        } else {
            Error(_W("Invalid value."));
        }
        std::wstring pathwstr;
        if (rootpath == L"." && path.wstring() != L".") {
            if (path.has_filename()) {
                pathwstr = path.parent_path().generic_wstring();
            } else {
                pathwstr = path.generic_wstring();
            }
        } else {
            if (rootpath != L"." && rootPath.has_filename()) {
                pathwstr = rootPath.parent_path().generic_wstring();
            } else {
                pathwstr = rootPath.generic_wstring();
            }
        }
        for (const auto& name : res) {
            localFiles.emplace_back(normalizeZipPath(name));
            std::wstring entry;
            if (StringHelpers::starts_with(name, pathwstr)) {
                entry = name.substr(pathwstr.size());
            }
            filesInZip.emplace_back(normalizeZipPath(entry));
        }
    }
}
//=============================================================================
void
Zip(const std::wstring& zipFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames)

{
    if (!rootpath.empty() && !FileSystemWrapper::Path::is_directory(rootpath)) {
        Error(_W("Invalid root path."));
    }
    wstringVector localFiles;
    wstringVector filesInZip;
    prepareFilesToZip(names, rootpath, localFiles, filesInZip);
    if (localFiles.empty()) {
        Error(_W("Nothing to zip."));
    }
    Nelson::Zipper zipFile;
    if (FileSystemWrapper::Path::is_regular_file(zipFilename)) {
        FileSystemWrapper::Path pathToRemove(zipFilename);
        FileSystemWrapper::Path::remove(pathToRemove);
    }
    zipFile.open(wstring_to_utf8(zipFilename).c_str(), false);
    if (!zipFile.isOpen()) {
        Error(_W("Cannot write file:") + L" " + zipFilename);
    }
    for (size_t k = 0; k < localFiles.size(); ++k) {
        std::wstring filename = localFiles[k];
        std::wstring entry = filesInZip[k];
        if (FileSystemWrapper::Path::is_directory(filename)) {
            uint32_t attributes;
            mz_os_get_file_attribs(wstring_to_utf8(filename).c_str(), &attributes);
            if (!StringHelpers::ends_with(entry, L"/")) {
                entry = entry + L"/";
            }
            if (StringHelpers::starts_with(entry, L"/")) {
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
            if (StringHelpers::starts_with(entry, L"/")) {
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
