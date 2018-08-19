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
#include "LocalFunctionsTable.hpp"
#include "BuiltInFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
LocalFunctionsTable::LocalFunctionsTable() { cachedLocalMacro.clear(); }
//=============================================================================
LocalFunctionsTable::~LocalFunctionsTable() { cachedLocalMacro.clear(); }
//=============================================================================
bool
LocalFunctionsTable::find(const std::string& key, FuncPtr& dest)
{
    std::unordered_map<std::string, FuncPtr>::const_iterator found = cachedLocalMacro.find(key);
    if (found != cachedLocalMacro.end()) {
        dest = found->second;
        return true;
    }
    return false;
}
//=============================================================================
bool
LocalFunctionsTable::add(const std::string& key, const FuncPtr val)
{
    FuncPtr v = nullptr;
    if (find(key, v)) {
        return false;
    }
    cachedLocalMacro.emplace(key, val);
    return true;
}
//=============================================================================
};
//=============================================================================
