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
#include "Zipper.hpp"
#include <mz_os.h>
#include <fstream>
#include "FileSystemWrapper.hpp"
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
} // namespace Nelson
//=============================================================================
