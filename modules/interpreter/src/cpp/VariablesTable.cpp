//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "VariablesTable.hpp"
#include "GenericTable.hpp"
#include "IsValidVariableName.hpp"
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
    lockedAccess = false;
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
ArrayOf*
VariablesTable::findVariable(const std::string& key)
{
    if (variablesTable != nullptr) {
        return ((GenericTable<ArrayOf>*)variablesTable)->findSymbol(key);
    }
    return nullptr;
}
//=============================================================================
bool
VariablesTable::findVariable(const key_type& key, value_type& dest)
{
    value_type* v = findVariable(key);
    if (v != nullptr) {
        dest = v[0];
        return true;
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
                while (lockedAccess) { }
                lockedAccess = true;
                genericTable->deleteSymbol(key);
                lockedAccess = false;
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
    if (key.empty()) {
        return false;
    }
    if (variablesTable == nullptr) {
        return false;
    }
    if (!IsValidVariableName(key, true)) {
        return false;
    }
    if (!isLockedVariable(key) || lockedVariables.empty()) {
        auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
        while (lockedAccess) { }
        lockedAccess = true;
        genericTable->insertSymbol(key, val);
        lockedAccess = false;
        return true;
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getVariablesList(bool withPersistent)
{
    if (variablesTable == nullptr) {
        return {};
    }
    auto* genericTable = (GenericTable<ArrayOf>*)variablesTable;
    stringVector list = genericTable->getAllSymbols();
    if (withPersistent) {
        return list;
    }
    stringVector retlist;

    for (auto e : list) {
        if (e.at(0) != '_') {
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
        lockedVariables.erase(
            std::find(lockedVariables.begin(), lockedVariables.end(), key)); //-V783
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
