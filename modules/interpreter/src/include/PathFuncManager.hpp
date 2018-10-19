//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <boost/container/vector.hpp>
#include <boost/unordered_map.hpp>
#include "FunctionDef.hpp"
#include "PathFunc.hpp"
#include "FileFunc.hpp"
#include "MacroFunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP PathFuncManager
{
private:
    boost::container::vector<PathFunc*> _pathFuncVector;
    // cache to speed up search
    boost::unordered_map<std::string, FuncPtr> cachedPathFunc;
    PathFuncManager();
    ~PathFuncManager();
    static PathFuncManager* m_pInstance;
    PathFunc* _userPath;
    MacroFunctionDef*
    processFile(std::wstring nlf_filename);

    void
    userpathCompute();
    std::wstring
    getPreferencesPath();
    std::wstring
    loadUserPathFromFile();
    bool
    saveUserPathToFile();
    bool
    isFile(std::wstring filename);
    bool
    isDir(std::wstring pathname);

public:
    static PathFuncManager*
    getInstance();
    void
    destroy();
    void
    clear();

    bool
    find(const std::string name, FuncPtr& ptr);
    bool
    find(const std::wstring functionName, std::wstring& filename);
    bool
    find(const std::wstring functionName, wstringVector& filesname);
    bool
    find(const std::wstring functionName, FileFunc** ff);
    bool
    find(size_t hashid, std::wstring& functionname);

    bool
    addPath(const std::wstring path, bool begin);
    bool
    removePath(const std::wstring path);
    wstringVector
    getPathNameVector();
    std::wstring
    getPathNameAsString();
    wstringVector
    getMacrosList(std::wstring prefix = L"");

    std::wstring
    getUserPath();
    bool
    setUserPath(const std::wstring path, bool saveToFile = false);
    void
    clearUserPath(bool saveToFile = false);
    void
    resetUserPath();
    void
    rehash();
    void
    rehash(const std::wstring path);

    void
    clearCache();
    void
    clearCache(stringVector exceptedFunctions);

    bool
    isPointerOnPathFunctionDef(FuncPtr ptr);
};
}
//=============================================================================
