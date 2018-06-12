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
#include <algorithm>
#include "VariablesTable.hpp"
//=============================================================================
#define SYMTAB 4096 * 2
//=============================================================================
namespace Nelson {
//=============================================================================
VariablesTable::VariablesTable()
{
    // variablesMap.reserve(SYMTAB);
    // lockedVariables.reserve(SYMTAB);
}
//=============================================================================
VariablesTable::~VariablesTable()
{
    variablesMap.clear();
    lockedVariables.clear();
}
//=============================================================================
bool
VariablesTable::findVariable(const key_type& key, value_type& dest)
{
    std::unordered_map<key_type, value_type>::iterator it = variablesMap.find(key);
    if (it != variablesMap.end()) {
        dest = it->second;
        return true;
    }
    return false;
}
//=============================================================================
bool
VariablesTable::isVariable(const key_type& key)
{
    if (variablesMap.count(key) > 0) {
        return true;
    }
    return false;
}
//=============================================================================
bool
VariablesTable::deleteVariable(const key_type& key)
{
    if (!isLockedVariable(key)) {
        if (isVariable(key)) {
            variablesMap.erase(key);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
VariablesTable::insertVariable(const key_type& key, const value_type& val)
{
    if (lockedVariables.empty()) {
        variablesMap[key] = val;
        return true;
    } else {
        // insert only in a not locked variable
        if (!isLockedVariable(key)) {
            variablesMap[key] = val;
            return true;
        }
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getVariablesList(bool withPersistent)
{
    stringVector retlist;
    for (auto it = variablesMap.begin(); it != variablesMap.end(); ++it) {
        if (!withPersistent) {
            if (it->first.at(0) != '_') {
                retlist.push_back(it->first);
            }
        } else {
            retlist.push_back(it->first);
        }
    }
    return retlist;
}
//=============================================================================
bool
VariablesTable::isLockedVariable(std::string key)
{
    if (!lockedVariables.empty()) {
        return (std::find(lockedVariables.begin(), lockedVariables.end(), key)
            != lockedVariables.end());
    }
    return false;
}
//=============================================================================
bool
VariablesTable::lockVariable(std::string key)
{
    if (!isLockedVariable(key)) {
        // ans cannot be locked
        if (key != "ans") {
            lockedVariables.push_back(key);
        }
        return true;
    } else {
        return true;
    }
    return false;
}
//=============================================================================
bool
VariablesTable::unlockVariable(std::string key)
{
    if (isLockedVariable(key)) {
        lockedVariables.erase(std::find(lockedVariables.begin(), lockedVariables.end(), key));
        return true;
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getLockedVariables()
{
    return lockedVariables;
}
//=============================================================================
}
//=============================================================================
