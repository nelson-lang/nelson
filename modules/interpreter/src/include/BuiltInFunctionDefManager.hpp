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
#include <vector>
#include <unordered_map>
#include <string>
#include "BuiltInFunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP BuiltInFunctionDefManager
{
public:
    static BuiltInFunctionDefManager*
    getInstance();
    bool
    add(std::string name, BuiltInFuncPtr fptr, int argc_in, int argc_out, std::wstring dynlibname,
        std::wstring modulename);

    bool
    remove(std::string name);
    bool
    remove(FuncPtr ptr);
    bool
    remove(BuiltInFunctionDef* ptr);
    bool
    remove(BuiltInFuncPtr fptr);
    bool
    removeAll();

    void
    destroy();

    std::vector<FuncPtr>
    getTable();
    stringVector
    getNameList();
    bool
    find(const std::string name, FuncPtr& ptr);
    bool
    find(const std::string name, wstringVector& paths);
    bool
    find(const std::string name, std::wstring& path);
    bool
    find(size_t hashid, std::wstring& functionname);

    bool
    isPointerOnBuiltInFunctionDef(FuncPtr ptr);

    void
    clearCache();

private:
    bool
    add(FuncPtr ptr);

    BuiltInFunctionDefManager();
    static BuiltInFunctionDefManager* m_pInstance;
    std::vector<FuncPtr> builtinVector;

    // cache to speed up search
    std::unordered_map<std::string, FuncPtr> cachedBuiltin;
};
}
//=============================================================================
