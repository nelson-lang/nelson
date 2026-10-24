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
#include <mutex>
#include <vector>
#include "FunctionDef.hpp"
#include "PathFunctionIndexer.hpp"
#include "FileFunction.hpp"
#include "MacroFunctionDef.hpp"
#include "MexFunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP PathFunctionIndexerManager
{
private:
    std::vector<PathFunctionIndexer*> _pathFuncVector;
    std::vector<PathFunctionIndexer*> _pathWatchFuncVector;
    std::unordered_map<std::string, FileFunction*> _pathFuncMap;
    // Guards the path tables against concurrent access: the interpreter thread
    // mutates them (addpath/rmpath/clear/rehash) while other threads (e.g. the
    // GUI file browser) enumerate them. Recursive because the public mutators
    // call one another (rehash -> clear/addPath, addPath -> refreshFunctionsMap).
    std::recursive_mutex m_mutex;

    PathFunctionIndexerManager();
    ~PathFunctionIndexerManager();
    static PathFunctionIndexerManager* m_pInstance;
    PathFunctionIndexer* _userPath;
    PathFunctionIndexer* _currentPath;
    // Indexes of the current directories cd left behind, most recent last.
    // run() moves into the script directory and back for every script, so
    // the directory it returns to is reused instead of being scanned again.
    std::vector<PathFunctionIndexer*> _currentPathCache;
    PathFunctionIndexer*
    takeCachedCurrentPath(const std::wstring& path);
    void
    cacheCurrentPath(PathFunctionIndexer* indexer);
    void
    clearCurrentPathCache();

    MexFunctionDef*
    processMexFile(const std::wstring& filename, const std::wstring& functionName, bool isOverload);

    MacroFunctionDef*
    processMacroFile(const std::wstring& script_filename, bool withWatcher, bool isOverload);

    FunctionDef*
    processFile(FileFunction* ff, const std::string& functionName);

    FunctionDef*
    findAndProcessFile(const std::string& name);

    bool
    find(const std::string& functionName, FileFunction** ff);

    // Called with m_mutex held; a null output only checks existence.
    bool
    findPackageClassFile(const std::string& functionName, std::wstring* filename);

    void
    userpathCompute();
    std::wstring
    loadUserPathFromFile();
    bool
    saveUserPathToFile();

    void
    refreshFunctionsMap();

    bool _filesWatcherStarted;
    bool _currentDirectorySearchEnabled = true;

public:
    static PathFunctionIndexerManager*
    getInstance();
    void
    destroy();
    void
    clear();

    bool
    find(const std::string& name, FunctionDefPtr& ptr);
    // Resolves a name against the directory indexes as they stand, without
    // going to the disk. Callers that have no other way left to resolve the
    // name follow a miss with rehashWatchedPathOwningFile().
    bool
    findIndexed(const std::string& name, FunctionDefPtr& ptr);
    // Probes the watched directories for a file defining the name and rehashes
    // the first one that owns it. Costs two file stats per watched directory,
    // so it belongs on a path that has already failed to resolve the name.
    bool
    rehashWatchedPathOwningFile(const std::string& functionName);
    bool
    find(const std::string& functionName, std::wstring& filename);
    // Does not load the function; indexed hits do not copy their path.
    bool
    functionFileExists(const std::string& functionName);
    bool
    find(const std::string& functionName, wstringVector& filenames);

    bool
    addPath(const std::wstring& path, bool begin, bool frozen);
    bool
    removePath(const std::wstring& path);
    wstringVector
    getPathNameVector(bool watchedOnly = false);
    std::wstring
    getPathNameAsString();
    wstringVector
    getMacrosList(bool withPrivate);

    bool
    setCurrentUserPath(const std::wstring& path);

    // Returns the previous setting; explicit addpath directories remain searchable.
    bool
    setCurrentDirectorySearchEnabled(bool enabled);

    std::wstring
    getUserPath();
    bool
    setUserPath(const std::wstring& path, bool saveToFile = false);
    void
    clearUserPath(bool saveToFile = false);
    void
    resetUserPath();
    void
    rehash();
    void
    rehash(const std::wstring& path);

    void
    clearCache();
    void
    clearCache(const stringVector& exceptedFunctions);

    bool
    isAvailablePath(const std::wstring& path);

    void
    startFileWatcher();

    bool
    hashOnFileWatcher();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
