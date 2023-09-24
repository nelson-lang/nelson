//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
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
    getFunctionsName(const std::wstring& prefix = L"");
    wstringVector
    getFunctionsFilename();
    std::wstring
    getPath();
    void
    rehash();
    bool
    findFuncName(const std::string& functionName, std::wstring& filename);
    std::unordered_map<std::string, FileFunction*>
    getAllFileFunctions();

    bool
    wasModified();

    void
    startFileWatcher();

    bool
    isWithWatcher();

private:
    std::unordered_map<std::string, FileFunction*> mapAllFiles;
    std::unordered_map<std::string, FileFunction*> mapRecentFiles;
    std::wstring _path;
    bool
    isSupportedFuncFilename(const std::wstring& name);
    bool
    comparePathname(const std::wstring& path1, const std::wstring& path2);

    void
    rehash(const std::wstring& pathToScan, const std::wstring& prefix, bool isPrivate);

    bool withWatcher;

    void* fileWatcher;
    void* updateFileWatcherListener;
    unsigned long watcherID;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
