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
#include <numeric>
#include <optional>
#include "Zip.hpp"
#include "ZipArchive.hpp"
#include "ZipHelpers.hpp"
#include "ArchiveInterruption.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
template <typename DirectoryIterator>
static void
appendMatchingFiles(const nfs::path& branch, const std::wstring& mask, wstringVector& res);
static wstringVector
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories);
static wstringVector
ListFiles(const std::wstring& directory, bool bSubdirectories);
static void
prepareFilesToZip(const wstringVector& names, const std::wstring& rootpath,
    wstringVector& localFiles, wstringVector& filesInZip);
static time_t
earliestZipTime();
//=============================================================================
void
Zip(const std::wstring& zipFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames)
{
    Zip(zipFilename, names, rootpath, entrynames, false);
}
//=============================================================================
void
collectArchiveItems(const wstringVector& names, const std::wstring& rootpath, bool reproducible,
    std::vector<ZipFileItem>& items, wstringVector& entrynames)
{
    wstringVector localFiles;
    wstringVector filesInZip;
    prepareFilesToZip(names, rootpath, localFiles, filesInZip);
    std::vector<size_t> order(localFiles.size());
    std::iota(order.begin(), order.end(), 0);
    if (reproducible) {
        std::sort(order.begin(), order.end(),
            [&](size_t left, size_t right) { return filesInZip[left] < filesInZip[right]; });
    }
    items.clear();
    items.reserve(order.size());
    std::vector<std::wstring> listed;
    for (size_t k : order) {
        const std::wstring& filename = localFiles[k];
        std::wstring entry = filesInZip[k];
        const bool isDirectory = FileSystemWrapper::Path::is_directory(filename);
        if (isDirectory && !StringHelpers::ends_with(entry, L"/")) {
            entry = entry + L"/";
        }
        if (StringHelpers::starts_with(entry, L"/")) {
            entry = entry.substr(1);
        }
        ZipFileItem item;
        item.name = wstring_to_utf8(entry);
        item.filename = filename;
        item.isDirectory = isDirectory;
        ZipArchive::fileMetadata(filename, item.options.modified, item.options.externalAttributes);
        if (reproducible) {
            item.options.modified = earliestZipTime();
        }
        items.push_back(std::move(item));
        // Like the reference, an empty folder is listed without its trailing '/'.
        listed.push_back(isDirectory ? entry.substr(0, entry.size() - 1) : entry);
    }
    std::sort(listed.begin(), listed.end());
    listed.erase(std::unique(listed.begin(), listed.end()), listed.end());
    entrynames = listed;
}
//=============================================================================
std::wstring
itemFilename(const std::vector<ZipFileItem>& items, const std::string& name)
{
    for (const ZipFileItem& item : items) {
        if (item.name == name) {
            return item.filename;
        }
    }
    return utf8_to_wstring(name);
}
//=============================================================================
void
Zip(const std::wstring& zipFilename, const wstringVector& names, const std::wstring& rootpath,
    wstringVector& entrynames, bool reproducible)
{
    if (!rootpath.empty() && !FileSystemWrapper::Path::is_directory(rootpath)) {
        Error(L"Nelson:zip:invalidRootPath", L"Invalid root path.");
    }
    std::vector<ZipFileItem> items;
    collectArchiveItems(names, rootpath, reproducible, items, entrynames);
    if (items.empty()) {
        Error(L"Nelson:zip:nothingToZip", L"Nothing to zip.");
    }
    if (reproducible && nfs::exists(nfs::path(zipFilename))) {
        Error(L"Nelson:nelsonc:archive", L"Application archive must not already exist.");
    }
    if (FileSystemWrapper::Path::is_regular_file(zipFilename)) {
        FileSystemWrapper::Path pathToRemove(zipFilename);
        FileSystemWrapper::Path::remove(pathToRemove);
    }
    std::optional<ZipArchive> opened;
    try {
        opened.emplace(ZipArchive::openFile(zipFilename, ZipArchive::Mode::Overwrite));
    } catch (const ZipArchiveError&) {
    }
    if (!opened) {
        Error(L"Nelson:zip:cannotWriteFile", L"Cannot write file: %s", zipFilename);
    }
    ZipArchive& zipFile = *opened;
    // Closes and deletes the partial archive before reporting an error.
    auto discardArchive = [&]() {
        try {
            zipFile.close();
        } catch (const ZipArchiveError&) {
        }
        std::error_code ignored;
        nfs::remove(nfs::path(zipFilename), ignored);
    };
    try {
        zipFile.addFiles(items);
    } catch (const ArchiveInterruption::Interrupted&) {
        discardArchive();
        throw;
    } catch (const ZipArchiveError& error) {
        discardArchive();
        if (error.code() == ZipErrorCode::OpenFailed || error.code() == ZipErrorCode::ReadFailed) {
            Error(L"Nelson:file_archiver:cannotReadFile", L"Cannot read file: %s",
                itemFilename(items, error.entry()));
        }
        Error(L"Nelson:zip:cannotWriteFile", L"Cannot write file: %s", zipFilename);
    }
    try {
        zipFile.close();
    } catch (const ZipArchiveError&) {
        discardArchive();
        Error(L"Nelson:zip:cannotWriteFile", L"Cannot write file: %s", zipFilename);
    }
}
//=============================================================================
template <typename DirectoryIterator>
static void
appendMatchingFiles(const nfs::path& branch, const std::wstring& mask, wstringVector& res)
{
    try {
        for (DirectoryIterator p(branch), end; p != end; ++p) {
            if (!wildcardMatch(p->path().filename().wstring(), mask)) {
                continue;
            }
            std::wstring file(p->path().wstring());
            if (file.size() > 1 && file[0] == L'.' && (file[1] == L'/' || file[1] == L'\\')) {
                file = file.substr(2);
            }
            FileSystemWrapper::Path current = file;
            res.push_back(current.generic_path().wstring());
        }
    } catch (const nfs::filesystem_error&) {
    }
}
//=============================================================================
static wstringVector
ListFilesWithWildcard(const std::wstring& mask, bool bSubdirectories)
{
    wstringVector res;
    nfs::path path(mask);
    if (nfs::exists(path)) {
        res.push_back(path.generic_wstring());
        return res;
    }
    nfs::path branch(path.parent_path());
    if (branch.empty()) {
        branch = nfs::current_path();
    }
    if (!nfs::is_directory(branch)) {
        return res;
    }
    const std::wstring filemask = path.filename().wstring();
    if (bSubdirectories) {
        appendMatchingFiles<nfs::recursive_directory_iterator>(branch, filemask, res);
    } else {
        appendMatchingFiles<nfs::directory_iterator>(branch, filemask, res);
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
    std::vector<bool> absolutes;
    FileSystemWrapper::Path rootPath = getRootPath(rootpath);

    for (const std::wstring& f : filteredNames) {
        FileSystemWrapper::Path p = f;
        absolutes.push_back(p.is_absolute());
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
            res = expandFolders(ListFiles(genericPath.generic_wstring(), false));
        } else if (isDir) {
            res = expandFolders({ genericPath.generic_wstring() });
        } else if (FileSystemWrapper::Path::is_regular_file(fullname.generic_wstring())) {
            res.emplace_back(fullname.generic_wstring());
        } else {
            Error(L"Nelson:zip:invalidValue", L"Invalid value.");
        }
        // Absolute names are stored relative to their parent folder, others to the root folder.
        const std::wstring pathwstr
            = absolutes[k] ? path.generic_wstring() : rootPath.generic_wstring();
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
// 1980-01-01 00:00 local time: the earliest ZIP date, independent of the clock.
static time_t
earliestZipTime()
{
    struct tm earliest = { };
    earliest.tm_year = 80;
    earliest.tm_mday = 1;
    earliest.tm_isdst = -1;
    return mktime(&earliest);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
