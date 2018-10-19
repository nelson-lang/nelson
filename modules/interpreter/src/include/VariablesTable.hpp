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
#include <string>
#include "ArrayOf.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
class VariablesTable
{
    //=============================================================================
private:
    typedef std::string key_type;
    typedef ArrayOf value_type;
    std::unordered_map<key_type, value_type> variablesMap;
    stringVector lockedVariables;
    //=============================================================================
public:
    VariablesTable();
    ~VariablesTable();
    bool
    findVariable(const key_type& key, value_type& dest);
    bool
    isVariable(const key_type& key);
    bool
    deleteVariable(const key_type& key);
    bool
    insertVariable(const key_type& key, const value_type& val);
    stringVector
    getVariablesList(bool withPersistent);
    bool
    isLockedVariable(std::string key);
    bool
    lockVariable(std::string key);
    bool
    unlockVariable(std::string key);
    stringVector
    getLockedVariables();
};
//=============================================================================
}
//=============================================================================
