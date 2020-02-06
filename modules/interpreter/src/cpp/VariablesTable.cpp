//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "VariablesTable.hpp"
#include "GenericTable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
VariablesTable::VariablesTable()
{
    GenericTable<ArrayOf>* genericTable = nullptr;
    try {
        genericTable = new GenericTable<ArrayOf>;
    } catch (const std::bad_alloc&) {
        genericTable = nullptr;
    }
    variablesTable = (void*)genericTable;
}
//=============================================================================
VariablesTable::~VariablesTable()
{
    if (variablesTable != nullptr) {
        auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
        delete genericTable;
        genericTable = nullptr;
    }
    lockedVariables.clear();
}
//=============================================================================
bool
VariablesTable::findVariable(const key_type& key, value_type& dest)
{
    if (variablesTable != nullptr) {
        auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
        value_type* v = genericTable->findSymbol(key);
        if (v != nullptr) {
            dest = v[0];
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
VariablesTable::isVariable(const key_type& key)
{
    if (variablesTable != nullptr) {
        auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
        value_type* v = genericTable->findSymbol(key);
        if (v != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
VariablesTable::deleteVariable(const key_type& key)
{
    if (!isLockedVariable(key)) {
        if (isVariable(key)) {
            if (variablesTable != nullptr) {
                auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
                genericTable->deleteSymbol(key);
                return true;
            }
        }
    }
    return false;
}
//=============================================================================
bool
VariablesTable::insertVariable(const key_type& key, const value_type& val)
{
    // insert only in a not locked variable
    if (!isLockedVariable(key) || lockedVariables.empty()) {
        if (variablesTable != nullptr) {
            auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
            genericTable->insertSymbol(key, val);
            return true;
        }
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getVariablesList(bool withPersistent)
{
    if (variablesTable == nullptr) {
        return stringVector();
    }
    auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
    stringVector list = genericTable->getAllSymbols();
    if (withPersistent) {
        return list;
    }
    stringVector retlist;

    for (auto e : list) {
        if (!withPersistent) {
            if (e.at(0) != '_') {
                retlist.push_back(e);
            }
        } else {
            retlist.push_back(e);
        }
    }
    return retlist;
}
//=============================================================================
bool
VariablesTable::isLockedVariable(const std::string& key)
{
    if (!lockedVariables.empty()) {
        return (std::find(lockedVariables.begin(), lockedVariables.end(), key)
            != lockedVariables.end());
    }
    return false;
}
//=============================================================================
bool
VariablesTable::lockVariable(const std::string& key)
{
    if (!isLockedVariable(key)) {
        // ans cannot be locked
        if (key != "ans") {
            lockedVariables.push_back(key);
        }
        return true;
    }
    return true;
}
//=============================================================================
bool
VariablesTable::unlockVariable(const std::string& key)
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
} // namespace Nelson
//=============================================================================
