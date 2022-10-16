//=============================================================================
// Copyright (c) 2016present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <fstream>
#include <boost/algorithm/string.hpp>
#include <boost/regex.hpp>
#include <mz_os.h>
#include "FileSystemHelpers.hpp"
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
    std::filesystem::path path = createFileSystemPath(mask);
    if (std::filesystem::exists(path)) {
        res.push_back(convertFileSytemPathToGenericWString(path));

    } else {
        std::filesystem::path branch(path.parent_path());
        if (branch.empty()) {
            branch = std::filesystem::current_path();
        }
        if (std::filesystem::is_directory(branch)) {
            std::wstring _mask = convertFileSytemPathToWString(path.filename());

            _mask = boost::regex_replace(_mask, boost::wregex(L"\\."), L"\\\\.");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\?"), L".");
            _mask = boost::regex_replace(_mask, boost::wregex(L"\\*"), L".*");

            boost::wregex rmask(_mask, boost::wregex::icase);
            if (bSubdirectories) {
                if (isDirectory(branch)) {
                    try {
                        for (std::filesystem::recursive_directory_iterator p(branch), end; p != end;
                             ++p) {
                            if (!boost::regex_match(
                                    convertFileSytemPathToWString(p->path().filename()), rmask)) {
                                continue;
                            }
                            std::wstring file(convertFileSytemPathToWString(p->path()));
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current = createFileSystemPath(file);
                            res.push_back(convertFileSytemPathToGenericWString(current));
                        }

                    } catch (const std::filesystem::filesystem_error&) {
                    }
                }

            } else {
                std::filesystem::path dir = branch;
                std::filesystem::path r = dir.root_path();
                if (isDirectory(branch)) {
                    try {
                        for (std::filesystem::directory_iterator p(branch), end; p != end; ++p) {
                            if (!boost::regex_match(
                                    convertFileSytemPathToWString(p->path().filename()), rmask)) {
                                continue;
                            }
                            std::wstring file(convertFileSytemPathToWString(p->path()));
                            if (file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                                file = std::wstring(file.begin() + 2, file.end());
                            }
                            std::filesystem::path current = createFileSystemPath(file);
                            res.push_back(convertFileSytemPathToGenericWString(current));
                        }

                    } catch (const std::filesystem::filesystem_error&) {
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
        if (isFile(directory)) {
            std::filesystem::path d = createFileSystemPath(directory);
            res.push_back(convertFileSytemPathToGenericWString(d));

        } else {
            if (directory.empty()) {
                res.clear();
                return res;
            }
            std::filesystem::path thispath = createFileSystemPath(directory);
            thispath = convertFileSytemPathToGenericWString(thispath);
            if (!boost::algorithm::ends_with(
                    convertFileSytemPathToGenericWString(thispath), L"/")) {
                thispath = convertFileSytemPathToGenericWString(thispath) + L"/";
            }
            std::filesystem::path branch(thispath.parent_path());
            if (branch.empty()) {
                branch = std::filesystem::current_path() / directory;

            } else {
                if (convertFileSytemPathToGenericWString(branch).back() == L':') {
                    branch = convertFileSytemPathToGenericWString(branch) + L"/";
                }
            }
            if (!std::filesystem::is_directory(branch)) {
                res.clear();
                return res;
            }
            if (bSubdirectories) {
                if (isDirectory(branch)) {
                    try {
                        for (std::filesystem::recursive_directory_iterator dir_iter(branch), end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(convertFileSytemPathToWString(current));
                        }

                    } catch (const std::filesystem::filesystem_error&) {
                    }
                }
            } else {
                std::filesystem::path dir = branch;
                std::filesystem::path r = dir.root_path();
                if (isDirectory(directory)) {
                    try {
                        for (std::filesystem::directory_iterator dir_iter(directory), end;
                             dir_iter != end; ++dir_iter) {
                            std::filesystem::path current = dir_iter->path();
                            res.push_back(convertFileSytemPathToGenericWString(current));
                        }

                    } catch (const std::filesystem::filesystem_error&) {
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
    std::filesystem::path rootPath = getRootPath(rootpath);

    for (const std::wstring& f : filteredNames) {
        std::filesystem::path p = createFileSystemPath(f);
        if (p.is_absolute()) {
            filenames.emplace_back(
                normalizeZipPath(convertFileSytemPathToGenericWString(p.filename())));
            paths.emplace_back(
                normalizeZipPath(convertFileSytemPathToGenericWString(p.parent_path())));

        } else {
            std::filesystem::path rp = rootpath;
            paths.emplace_back(normalizeZipPath(convertFileSytemPathToGenericWString(rp)));
            std::filesystem::path fp = createFileSystemPath(f);
            filenames.emplace_back(normalizeZipPath(convertFileSytemPathToGenericWString(fp)));
        }
    }
    for (size_t k = 0; k < filenames.size(); ++k) {
        std::filesystem::path fullname;
        std::filesystem::path path = createFileSystemPath(paths[k]);
        std::filesystem::path file = createFileSystemPath(filenames[k]);
        if (path == L".") {
            fullname = std::filesystem::current_path() / file;
        } else {
            fullname = path / file;
        }
        std::filesystem::path genericPath
            = createFileSystemPath(convertFileSytemPathToGenericWString(fullname));
        bool isDir = isDirectory(convertFileSytemPathToGenericWString(genericPath));
        bool hasStar
            = boost::algorithm::contains(convertFileSytemPathToGenericWString(genericPath), "*")
            || boost::algorithm::contains(convertFileSytemPathToGenericWString(genericPath), "?");
        wstringVector res;
        if (hasStar) {
            res = ListFiles(convertFileSytemPathToGenericWString(genericPath), false);
        } else if (isDir) {
            res = ListFiles(convertFileSytemPathToGenericWString(genericPath), true);
        } else if (isFile(convertFileSytemPathToGenericWString(fullname))) {
            res.emplace_back(convertFileSytemPathToGenericWString(fullname));
        } else {
            Error(_W("Invalid value."));
        }
        std::wstring pathwstr;
        if (rootpath == L"." && path != L".") {
            pathwstr = convertFileSytemPathToGenericWString(path);
        } else {
            pathwstr = convertFileSytemPathToGenericWString(rootPath);
        }
        for (const auto& name : res) {
            localFiles.emplace_back(normalizeZipPath(name));
            std::wstring entry;
            if (boost::algorithm::starts_with(name, pathwstr)) {
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
    if (!rootpath.empty() && !isDirectory(rootpath)) {
        Error(_W("Invalid root path."));
    }
    wstringVector localFiles;
    wstringVector filesInZip;
    prepareFilesToZip(names, rootpath, localFiles, filesInZip);
    if (localFiles.empty()) {
        Error(_W("Nothing to zip."));
    }
    Nelson::Zipper zipFile;
    if (isFile(zipFilename)) {
        std::filesystem::path pathToRemove = createFileSystemPath(zipFilename);
        std::filesystem::remove(pathToRemove);
    }
    zipFile.open(wstring_to_utf8(zipFilename).c_str(), false);
    if (!zipFile.isOpen()) {
        Error(_W("Cannot write file:") + L" " + zipFilename);
    }
    for (size_t k = 0; k < localFiles.size(); ++k) {
        std::wstring filename = localFiles[k];
        std::wstring entry = filesInZip[k];
        if (isDirectory(filename)) {
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
