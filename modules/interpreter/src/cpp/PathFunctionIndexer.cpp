//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "nlsBuildConfig.h"
#include "StringHelpers.hpp"
#include "PathFunctionIndexer.hpp"
#include "BytecodeArtifact.hpp"
#include "characters_encoding.hpp"
#include "MxGetExtension.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "FileSystemWrapper.hpp"
#include "OverloadName.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
PathFunctionIndexer::PathFunctionIndexer(const std::wstring& path, bool withWatcher)
{
    this->withWatcher = withWatcher;
    this->watching = false;
    if (FileSystemWrapper::Path::is_directory(path)) {
        _path = path;
    } else {
        _path.clear();
    }
    rehash();
}
//=============================================================================
bool
PathFunctionIndexer::isWithWatcher() const
{
    return NelsonConfiguration::getInstance()->isFileWatcherEnabled() && this->withWatcher;
}
//=============================================================================
void
PathFunctionIndexer::startFileWatcher()
{
#if WITH_FILE_WATCHER
    if (NelsonConfiguration::getInstance()->isFileWatcherEnabled() && withWatcher) {
        recordLastWriteTime();
        watching = true;
    }
#endif
}
//=============================================================================
std::unordered_map<std::string, FileFunction*>
PathFunctionIndexer::getAllFileFunctions() const
{
    return mapAllFiles;
}
//=============================================================================
bool
PathFunctionIndexer::wasModified()
{
#if WITH_FILE_WATCHER
    if (!watching || !withWatcher || !NelsonConfiguration::getInstance()->isFileWatcherEnabled()) {
        return false;
    }
    std::error_code errorCode;
    const nfs::file_time_type currentWriteTime = nfs::last_write_time(_path, errorCode);
    if (errorCode) {
        // The directory is gone or unreadable; rmpath and rehash deal with that.
        return false;
    }
    if (currentWriteTime == lastWriteTime) {
        return false;
    }
    lastWriteTime = currentWriteTime;
    return true;
#else
    return false;
#endif
}
//=============================================================================
bool
PathFunctionIndexer::directoryUnchanged() const
{
    std::error_code errorCode;
    const nfs::file_time_type currentWriteTime = nfs::last_write_time(_path, errorCode);
    return !errorCode && currentWriteTime == lastWriteTime;
}
//=============================================================================
void
PathFunctionIndexer::recordLastWriteTime()
{
    std::error_code errorCode;
    const nfs::file_time_type writeTime = nfs::last_write_time(_path, errorCode);
    lastWriteTime = errorCode ? nfs::file_time_type {} : writeTime;
}
//=============================================================================
bool
PathFunctionIndexer::comparePathname(const std::wstring& path1, const std::wstring& path2)
{
    FileSystemWrapper::Path p1(path1);
    FileSystemWrapper::Path p2(path2);
    return FileSystemWrapper::Path::equivalent(p1, p2);
}
//=============================================================================
PathFunctionIndexer::~PathFunctionIndexer()
{
    for (const auto& mapAllFile : mapAllFiles) {
        delete mapAllFile.second;
    }
    mapRecentFiles.clear();
    mapAllFiles.clear();
    _path.clear();
}
//=============================================================================
wstringVector
PathFunctionIndexer::getFunctionsName(const std::wstring& prefix, bool withPrivate) const
{
    wstringVector functionsName;
    functionsName.reserve(mapAllFiles.size());

    const bool hasPrefix = !prefix.empty();

    for (const auto& mapAllFile : mapAllFiles) {
        const FileFunction* ff = mapAllFile.second;
        if (!ff) {
            continue;
        }

        if (!withPrivate && ff->isPrivate()) {
            continue;
        }

        const std::wstring& name = ff->getName();
        if (!hasPrefix || StringHelpers::starts_with(name, prefix)) {
            functionsName.emplace_back(name);
        }
    }
    return functionsName;
}
//=============================================================================
wstringVector
PathFunctionIndexer::getFunctionsFilename() const
{
    wstringVector functionsFilename;
    functionsFilename.reserve(mapAllFiles.size());

    for (const auto& mapAllFile : mapAllFiles) {
        if (mapAllFile.second) {
            functionsFilename.emplace_back(mapAllFile.second->getFilename());
        }
    }
    return functionsFilename;
}
//=============================================================================
const std::wstring&
PathFunctionIndexer::getPath() const
{
    return _path;
}
//=============================================================================
bool
PathFunctionIndexer::isSupportedFuncFilename(const std::wstring& name)
{
    for (wchar_t c : name) {
        if (!((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z') || c == L'_'
                || (c >= L'0' && c <= L'9'))) {
            return false;
        }
    }
    return true;
}
//=============================================================================
inline std::wstring
getFunctionName(const std::wstring& objectName, const std::wstring& name)
{
    if (objectName.empty() || objectName == name) {
        return name;
    }
    const size_t dotPosition = objectName.find_last_of(L'.');
    if (dotPosition != std::wstring::npos && dotPosition + 1 < objectName.size()
        && objectName.substr(dotPosition + 1) == name) {
        return objectName;
    }
    return L"@" + objectName + L"/" + name;
}
//=============================================================================
inline std::wstring
qualifiedPackageName(const std::wstring& packagePrefix, const std::wstring& name)
{
    if (packagePrefix.empty()) {
        return name;
    }
    return packagePrefix + L"." + name;
}
//=============================================================================
void
PathFunctionIndexer::rehash(const std::wstring& pathToScan, const std::wstring& prefix,
    bool isPrivate, const std::wstring& packagePrefix)
{
    try {
        const FileSystemWrapper::Path path(pathToScan);
        if (!path.is_directory()) {
            return;
        }

        const std::wstring mexExt = L"." + getMexExtension();
        const std::wstring macroExt = L".m";
        const std::wstring privateDir = L"private";

        std::wstring objectName;
        if (prefix.length() > 1) {
            objectName = prefix.substr(1);
            objectName = qualifiedPackageName(packagePrefix, objectName);
        }

        nfs::directory_iterator end_iter;
        for (nfs::directory_iterator dir_iter(path.native()); dir_iter != end_iter; ++dir_iter) {
            const FileSystemWrapper::Path current(dir_iter->path().native());

            // The entry carries the attributes the enumeration returned;
            // asking the file again would cost one stat per entry.
            std::error_code entryError;
            if (dir_iter->is_directory(entryError)) {
                const std::wstring stemDirectory = current.stem().wstring();
                if (!stemDirectory.empty()) {
                    if (stemDirectory[0] == OVERLOAD_SYMBOL_CHAR) {
                        rehash(current.generic_wstring(), stemDirectory, false, packagePrefix);
                    } else if (stemDirectory[0] == L'+') {
                        const std::wstring packageName = stemDirectory.substr(1);
                        if (isSupportedFuncFilename(packageName)) {
                            rehash(current.generic_wstring(), L"", false,
                                qualifiedPackageName(packagePrefix, packageName));
                        }
                    } else if (stemDirectory == privateDir) {
                        rehash(current.generic_wstring(), L"", true, packagePrefix);
                    }
                }
                continue;
            }

            const std::wstring ext = current.extension().generic_wstring();
            const bool isMacro = (ext == macroExt);
            const bool isMex = (ext == mexExt);
            const bool isBytecode = !isMacro && !isMex && ext == L".nbc";

            if (!(isMacro || isMex || isBytecode)) {
                continue;
            }

            std::wstring stemName = current.stem().generic_wstring();
            if (!isSupportedFuncFilename(stemName)) {
                continue;
            }
            const std::wstring localStemName = stemName;
            if (prefix.empty()) {
                stemName = qualifiedPackageName(packagePrefix, stemName);
            }

            const std::wstring name = getFunctionName(objectName, stemName);
            const std::wstring pathName = prefix.empty()
                ? pathToScan
                : nfs::path(pathToScan).parent_path().generic_wstring();

            bool isOverload = !prefix.empty() && !isPrivate;
            if (!prefix.empty() && prefix.substr(1) == localStemName) {
                isOverload = false;
            }

            try {
                auto ff = std::make_unique<FileFunction>(
                    pathName, objectName, name, isMex, withWatcher, isOverload, isPrivate);
                if (isBytecode) {
                    ff->setFilename(current.generic_wstring());
                }

                const std::wstring keyName = isPrivate && prefix.empty() ? localStemName : name;
                const std::string key = isPrivate ? wstring_to_utf8(pathName + L"/" + keyName)
                                                  : wstring_to_utf8(name);

                const auto inserted = mapAllFiles.emplace(key, ff.get());
                if (inserted.second) {
                    ff.release();
                } else {
                    const auto* previous = inserted.first->second;
                    const int previousRank = previous->isMex()        ? 3
                        : isBytecodeArtifact(previous->getFilename()) ? 1
                                                                      : 2;
                    const int rank = isMex ? 3 : isMacro ? 2 : 1;
                    if (rank > previousRank) {
                        delete inserted.first->second;
                        inserted.first->second = ff.release();
                    }
                }
            } catch (const std::bad_alloc&) {
                // The entry is skipped.
            }
        }
    } catch (const nfs::filesystem_error&) {
        // An unreadable directory contributes no entries.
    }
}
//=============================================================================
void
PathFunctionIndexer::rehash()
{
    if (_path.empty()) {
        return;
    }
    // Read the reference before the scan: a file written while it runs then
    // shows up as a change instead of being missed.
    recordLastWriteTime();

    for (const auto& pair : mapAllFiles) {
        delete pair.second;
    }
    mapAllFiles.clear();
    mapRecentFiles.clear();

    mapAllFiles.reserve(128);

    rehash(_path, L"", false);
}
//=============================================================================
bool
PathFunctionIndexer::findFuncName(
    const std::string& functionName, std::wstring& filename, bool probeDisk) const
{
    const auto found = mapAllFiles.find(functionName);
    if (found != mapAllFiles.end()) {
        filename = found->second->getFilename();
        if (FileSystemWrapper::Path::is_regular_file(filename)) {
            return true;
        }
    }

    if (!probeDisk || !withWatcher) {
        return false;
    }

    const std::wstring wFunctionName = utf8_to_wstring(functionName);

    filename = _path + L"/" + wFunctionName + L"." + getMexExtension();
    if (FileSystemWrapper::Path::is_regular_file(filename)) {
        return true;
    }

    filename = _path + L"/" + wFunctionName + L".m";
    if (FileSystemWrapper::Path::is_regular_file(filename)) {
        return true;
    }
    filename = _path + L"/" + wFunctionName + L".nbc";
    return FileSystemWrapper::Path::is_regular_file(filename);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
