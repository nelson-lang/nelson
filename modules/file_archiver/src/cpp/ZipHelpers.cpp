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
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <sys/utime.h>
#else
#include <utime.h>
#endif
#include <ctime>
#include <cwctype>
#include <fstream>
#include "FileSystemWrapper.hpp"
#include <filesystem>
#include "ZipHelpers.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
#include "Error.hpp"
#include "StringHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::wstring
normalizeZipPath(const std::wstring& path)
{
    FileSystemWrapper::Path p(path);
    p = p.generic_path().lexically_normal();
    if (StringHelpers::starts_with(p.wstring(), L"./")) {
        p = p.wstring().substr(2);
    }
    return p.generic_wstring();
}
//=============================================================================
bool
wildcardMatch(const std::wstring& name, const std::wstring& mask)
{
    // Greedy scan that backtracks to the last '*' on a mismatch.
    size_t n = 0, m = 0;
    size_t starMask = std::wstring::npos, starName = 0;
    while (n < name.size()) {
        if (m < mask.size()
            && (mask[m] == L'?' || std::towlower(mask[m]) == std::towlower(name[n]))) {
            ++n;
            ++m;
        } else if (m < mask.size() && mask[m] == L'*') {
            starMask = m++;
            starName = n;
        } else if (starMask != std::wstring::npos) {
            m = starMask + 1;
            n = ++starName;
        } else {
            return false;
        }
    }
    while (m < mask.size() && mask[m] == L'*') {
        ++m;
    }
    return m == mask.size();
}
//=============================================================================
std::wstring
getRootPath(const std::wstring& rootpath)
{
    FileSystemWrapper::Path p;
    if (rootpath == L".") {
        p = FileSystemWrapper::Path::current_path();
    } else {
        p = FileSystemWrapper::Path(rootpath);
    }
    return normalizeZipPath(p.wstring());
}
//=============================================================================
bool
resolveSafeZipEntryPath(
    const std::wstring& fullRootPath, const std::wstring& entryName, std::wstring& resolvedPath)
{
    if (entryName.empty()) {
        return false;
    }
    // Reject backslash separators: the ZIP format mandates '/', and a backslash
    // is a Windows-specific traversal / bypass vector.
    if (entryName.find(L'\\') != std::wstring::npos) {
        return false;
    }
    // Reject drive designators and NTFS alternate data streams ("C:...", "x:y").
    if (entryName.find(L':') != std::wstring::npos) {
        return false;
    }
    if (entryName.front() == L'/') {
        return false;
    }
    // Reject any ".." path component (directory traversal).
    size_t start = 0;
    while (start < entryName.size()) {
        size_t slash = entryName.find(L'/', start);
        std::wstring component = (slash == std::wstring::npos)
            ? entryName.substr(start)
            : entryName.substr(start, slash - start);
        if (component == L"..") {
            return false;
        }
        if (slash == std::wstring::npos) {
            break;
        }
        start = slash + 1;
    }
    // Final containment guard: the normalized destination must stay under the
    // root directory (defends against any traversal the checks above missed).
    FileSystemWrapper::Path candidate(fullRootPath + L"/" + entryName);
    std::wstring normalized = candidate.generic_path().lexically_normal().generic_wstring();
    std::wstring rootPrefix = fullRootPath;
    if (rootPrefix.empty() || rootPrefix.back() != L'/') {
        rootPrefix += L'/';
    }
    std::wstring normalizedWithSep = normalized;
    if (normalizedWithSep.empty() || normalizedWithSep.back() != L'/') {
        normalizedWithSep += L'/';
    }
    if (!StringHelpers::starts_with(normalizedWithSep, rootPrefix)) {
        return false;
    }
    resolvedPath = normalized;
    return true;
}
//=============================================================================
std::wstring
canonicalFolder(const std::wstring& folder)
{
    std::error_code ec;
    const nfs::path canonical = nfs::weakly_canonical(nfs::path(folder), ec);
    return ec ? folder : canonical.generic_wstring();
}
//=============================================================================
std::wstring
diskPath(const std::wstring& canonicalRoot, const std::wstring& fullRootPath,
    const std::wstring& safePath)
{
    if (safePath.compare(0, fullRootPath.size(), fullRootPath) != 0) {
        return safePath;
    }
    std::wstring rest = safePath.substr(fullRootPath.size());
    while (!rest.empty() && rest.front() == L'/') {
        rest.erase(0, 1);
    }
    return rest.empty() ? canonicalRoot : canonicalRoot + L"/" + rest;
}
//=============================================================================
bool
linkStaysInside(const std::string& entryName, const std::string& target)
{
    if (target.empty() || target.front() == '/' || target.find(':') != std::string::npos
        || target.find('\\') != std::string::npos) {
        return false;
    }
    const nfs::path link = nfs::u8path(entryName).parent_path() / nfs::u8path(target);
    const nfs::path normalized = link.lexically_normal();
    return !normalized.empty() && *normalized.begin() != "..";
}
//=============================================================================
// Replaces each folder by its files and empty subfolders (an empty folder is kept itself).
wstringVector
expandFolders(const wstringVector& paths)
{
    wstringVector result;
    for (const std::wstring& path : paths) {
        std::error_code ec;
        const nfs::path folder(path);
        if (!nfs::is_directory(folder, ec)) {
            result.push_back(path);
            continue;
        }
        if (nfs::is_empty(folder, ec)) {
            result.push_back(folder.generic_wstring());
            continue;
        }
        for (nfs::recursive_directory_iterator it(folder, ec), end; !ec && it != end;
            it.increment(ec)) {
            const nfs::path& current = it->path();
            std::error_code status;
            if (nfs::is_directory(current, status)) {
                if (nfs::is_empty(current, status)) {
                    result.push_back(current.generic_wstring());
                }
            } else {
                result.push_back(current.generic_wstring());
            }
        }
    }
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
