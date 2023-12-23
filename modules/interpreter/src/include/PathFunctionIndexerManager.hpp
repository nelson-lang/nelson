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

    PathFunctionIndexerManager();
    ~PathFunctionIndexerManager();
    static PathFunctionIndexerManager* m_pInstance;
    PathFunctionIndexer* _userPath;
    PathFunctionIndexer* _currentPath;

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

    void
    userpathCompute();
    std::wstring
    loadUserPathFromFile();
    bool
    saveUserPathToFile();

    void
    refreshFunctionsMap();

    bool _filesWatcherStarted;

public:
    static PathFunctionIndexerManager*
    getInstance();
    void
    destroy();
    void
    clear();

    bool
    find(const std::string& name, FunctionDefPtr& ptr);
    bool
    find(const std::string& functionName, std::wstring& filename);
    bool
    find(const std::string& functionName, wstringVector& filesname);

    bool
    addPath(const std::wstring& path, bool begin, bool frozen);
    bool
    removePath(const std::wstring& path);
    wstringVector
    getPathNameVector(bool watchedOnly = false);
    std::wstring
    getPathNameAsString();
    wstringVector
    getMacrosList(const std::wstring& prefix = L"");

    bool
    setCurrentUserPath(const std::wstring& path);

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
