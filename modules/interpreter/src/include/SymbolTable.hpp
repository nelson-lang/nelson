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
#include <unordered_map>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include "Types.hpp"
//=============================================================================
#define SYMTAB 4096 * 2
//=============================================================================
namespace Nelson {
template <class T> class SymbolTable
{
    //=============================================================================
private:
    typedef std::string key_type;
    typedef T value_type;
    std::vector<std::pair<key_type, value_type>> symbolMap;
    //=============================================================================
public:
    //=============================================================================
    std::vector<std::pair<key_type, value_type>>
    getMap()
    {
        return symbolMap;
    }
    //=============================================================================
    SymbolTable()
    {
        // symbolMap.reserve(SYMTAB);
    }
    //=============================================================================
    ~SymbolTable() { symbolMap.clear(); }
    //=============================================================================
    bool
    findSymbol(const key_type key, value_type& dest)
    {
        auto it = std::find_if(symbolMap.begin(), symbolMap.end(),
            [&key](std::pair<key_type, value_type> const& elem) { return elem.first == key; });
        if (it != symbolMap.end()) {
            dest = it->second;
            return true;
        }
        return false;
    }
    //=============================================================================
    void
    deleteSymbol(const key_type& key)
    {
        auto it = std::find_if(symbolMap.begin(), symbolMap.end(),
            [&key](std::pair<key_type, value_type> const& elem) { return elem.first == key; });
        if (it != symbolMap.end()) {
            symbolMap.erase(it);
        }
    }
    //=============================================================================
    void
    deleteAllSymbols()
    {
        symbolMap.clear();
        // symbolMap.reserve(SYMTAB);
    }
    //=============================================================================
    void
    insertSymbol(const key_type& key, const value_type& val)
    {
        symbolMap.push_back(std::make_pair(key, val));
    }
    //=============================================================================
    stringVector
    getMacrosList()
    {
        stringVector retlist;
        for (auto it = symbolMap.begin(); it != symbolMap.end(); ++it) {
            retlist.push_back(it->first);
        }
        return retlist;
    }
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
