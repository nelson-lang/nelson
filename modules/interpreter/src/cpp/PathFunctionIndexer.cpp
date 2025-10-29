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
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "nlsBuildConfig.h"
#if WITH_FILE_WATCHER
#include <FileWatcher/FileWatcher.h>
#include <mutex>
#endif
#include "StringHelpers.hpp"
#include "PathFunctionIndexer.hpp"
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
#if WITH_FILE_WATCHER
class UpdateListener : public FW::FileWatchListener
{
public:
    UpdateListener() { updated = false; }
    void
    handleFileAction(
        FW::WatchID watchid, const FW::String& dir, const FW::String& filename, FW::Action action)
    {
        lock();
        updated = true;
    }

    bool
    readLastState()
    {
        lock();
        if (updated) {
            updated = false;
            return true;
        }
        return false;
    }

private:
    bool updated;
    std::mutex m_mutex;
    void
    lock()
    {
        std::scoped_lock<std::mutex> _lock { m_mutex };
    }
};
#endif
//=============================================================================
PathFunctionIndexer::PathFunctionIndexer(const std::wstring& path, bool withWatcher)
{
    this->withWatcher = withWatcher;
    fileWatcher = nullptr;
    watcherID = 0;
    updateFileWatcherListener = nullptr;
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
    if (NelsonConfiguration::getInstance()->isFileWatcherEnabled() && withWatcher && !fileWatcher
        && !updateFileWatcherListener) {
        FW::FileWatcher* _fileWatcher = new FW::FileWatcher();
        UpdateListener* _updateListener = new UpdateListener();
        fileWatcher = (void*)(_fileWatcher);
        updateFileWatcherListener = (void*)(_updateListener);
#if _MSC_VER
        watcherID = _fileWatcher->addWatch(_path, _updateListener, false);
#else
        watcherID = _fileWatcher->addWatch(wstring_to_utf8(_path), _updateListener, false);
#endif
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
PathFunctionIndexer::wasModified() const
{
#if WITH_FILE_WATCHER
    if (NelsonConfiguration::getInstance()->isFileWatcherEnabled() && withWatcher) {
        if (fileWatcher && updateFileWatcherListener) {
            static_cast<FW::FileWatcher*>(fileWatcher)->update();
            return static_cast<UpdateListener*>(updateFileWatcherListener)->readLastState();
        }
    }
#endif
    return false;
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
#if WITH_FILE_WATCHER
    if (fileWatcher) {
        FW::FileWatcher* _fileWatcher = static_cast<FW::FileWatcher*>(fileWatcher);
        _fileWatcher->removeWatch(watcherID);
        delete _fileWatcher;
        fileWatcher = nullptr;
    }
    if (updateFileWatcherListener) {
        UpdateListener* _updateFileWatcherListener
            = static_cast<UpdateListener*>(updateFileWatcherListener);
        delete _updateFileWatcherListener;
        updateFileWatcherListener = nullptr;
    }
#endif
}
//=============================================================================
wstringVector
PathFunctionIndexer::getFunctionsName(const std::wstring& prefix, bool withPrivate) const
{
    wstringVector functionsName;
    functionsName.reserve(mapAllFiles.size()); // Reserve memory to avoid reallocations

    const bool hasPrefix = !prefix.empty();

    for (const auto& mapAllFile : mapAllFiles) {
        const FileFunction* ff = mapAllFile.second;
        if (!ff)
            continue;

        if (!withPrivate && ff->isPrivate())
            continue;

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
    // Use range-based for with auto for better performance
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
    return L"@" + objectName + L"/" + name;
}
//=============================================================================
void
PathFunctionIndexer::rehash(
    const std::wstring& pathToScan, const std::wstring& prefix, bool isPrivate)
{
    try {
        const FileSystemWrapper::Path path(pathToScan);
        if (!path.is_directory())
            return;

        // Pre-compute commonly used values
        const std::wstring mexExt = L"." + getMexExtension();
        const std::wstring macroExt = L".m";
        const std::wstring privateDir = L"private";

        std::wstring objectName;
        if (prefix.length() > 1) {
            objectName = prefix.substr(1); // More efficient than erase
        }

        nfs::directory_iterator end_iter;
        for (nfs::directory_iterator dir_iter(path.native()); dir_iter != end_iter; ++dir_iter) {
            const FileSystemWrapper::Path current(dir_iter->path().native());

            if (current.is_directory()) {
                const std::wstring stemDirectory = current.stem().wstring();
                if (!stemDirectory.empty()) {
                    if (stemDirectory[0] == OVERLOAD_SYMBOL_CHAR) {
                        rehash(current.generic_wstring(), stemDirectory, false);
                    } else if (stemDirectory == privateDir) {
                        rehash(current.generic_wstring(), L"", true);
                    }
                }
                continue;
            }

            const std::wstring ext = current.extension().generic_wstring();
            const bool isMacro = (ext == macroExt);
            const bool isMex = (ext == mexExt);

            if (!(isMacro || isMex))
                continue;

            const std::wstring stemName = current.stem().generic_wstring();
            if (!isSupportedFuncFilename(stemName))
                continue;

            const std::wstring name = getFunctionName(objectName, stemName);
            const std::wstring pathName = prefix.empty()
                ? pathToScan
                : nfs::path(pathToScan).parent_path().generic_wstring();

            bool isOverload = !prefix.empty() && !isPrivate;
            if (!prefix.empty() && prefix.substr(1) == name) {
                isOverload = false;
            }

            try {
                auto ff = std::make_unique<FileFunction>(
                    pathName, objectName, name, isMex, withWatcher, isOverload, isPrivate);

                const std::string key
                    = isPrivate ? wstring_to_utf8(pathName + L"/" + name) : wstring_to_utf8(name);

                mapAllFiles.emplace(std::move(key), ff.release());
            } catch (const std::bad_alloc&) {
                // Handle allocation failure gracefully
            }
        }
    } catch (const nfs::filesystem_error&) {
        // Handle filesystem errors gracefully
    }
}
//=============================================================================
void
PathFunctionIndexer::rehash()
{
    if (_path.empty())
        return;

    // Clear existing data
    for (const auto& pair : mapAllFiles) {
        delete pair.second;
    }
    mapAllFiles.clear();
    mapRecentFiles.clear();

    // Reserve some space to reduce rehashing
    mapAllFiles.reserve(128);

    rehash(_path, L"", false);
}
//=============================================================================
bool
PathFunctionIndexer::findFuncName(const std::string& functionName, std::wstring& filename) const
{
    // Use const iterator for better performance
    const auto found = mapAllFiles.find(functionName);
    if (found != mapAllFiles.end()) {
        filename = found->second->getFilename();
        if (FileSystemWrapper::Path::is_regular_file(filename)) {
            return true;
        }
    }

    if (!withWatcher)
        return false;

    // Cache the converted function name to avoid repeated conversions
    const std::wstring wFunctionName = utf8_to_wstring(functionName);

    // Check for MEX file
    filename = _path + L"/" + wFunctionName + L"." + getMexExtension();
    if (FileSystemWrapper::Path::is_regular_file(filename)) {
        return true;
    }

    // Check for macro file
    filename = _path + L"/" + wFunctionName + L".m";
    return FileSystemWrapper::Path::is_regular_file(filename);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
