//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <unordered_map>
#include <string>
#include <filesystem>
#include "FileFunction.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class PathFunctionIndexer
{
public:
    PathFunctionIndexer(const std::wstring& path, bool withWatcher = true);
    ~PathFunctionIndexer();
    wstringVector
    getFunctionsName(const std::wstring& prefix, bool withPrivate) const;
    wstringVector
    getFunctionsFilename() const;
    const std::wstring&
    getPath() const;
    void
    rehash();
    // probeDisk asks for the two file stats that find a file written to the
    // directory since the last rehash. Without it the answer comes from the
    // index alone and the call touches the disk at most once.
    bool
    findFuncName(const std::string& functionName, std::wstring& filename, bool probeDisk) const;
    std::unordered_map<std::string, FileFunction*>
    getAllFileFunctions() const;

    // True once the directory has changed since the last rehash, and resets
    // the reference so that the next call only reports the next change.
    bool
    wasModified();

    // True while the directory has not changed since the last rehash, so the
    // index still lists its entries. Does not depend on the file watcher.
    bool
    directoryUnchanged() const;

    // Starts reporting changes. Until it is called the directory is indexed
    // but never re-read, which is what the modules want: they are added to the
    // path before the interpreter is ready to answer anything.
    void
    startFileWatcher();

    bool
    isWithWatcher() const;

private:
    std::unordered_map<std::string, FileFunction*> mapAllFiles;
    std::unordered_map<std::string, FileFunction*> mapRecentFiles;
    std::wstring _path;
    bool
    isSupportedFuncFilename(const std::wstring& name);
    bool
    comparePathname(const std::wstring& path1, const std::wstring& path2);

    void
    rehash(const std::wstring& pathToScan, const std::wstring& prefix, bool isPrivate,
        const std::wstring& packagePrefix = L"");

    void
    recordLastWriteTime();

    bool withWatcher;
    // A directory is watched by its modification time: creating, removing or
    // renaming an entry in it moves that time. Recorded at every rehash.
    bool watching;
    std::filesystem::file_time_type lastWriteTime;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
