//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "VariablesTable.hpp"
#include "IsValidVariableName.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
VariablesTable::VariablesTable() { }
//=============================================================================
VariablesTable::~VariablesTable() { lockedVariables.clear(); }
//=============================================================================
ArrayOf*
VariablesTable::findVariable(const std::string& key)
{
    return (variablesTable).findSymbol(key);
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
    value_type* v = variablesTable.findSymbol(key);
    if (v != nullptr) {
        return true;
    }
    return false;
}
//=============================================================================
bool
VariablesTable::deleteVariable(const key_type& key)
{
    std::lock_guard<std::mutex> lock(accessMutex);
    if (!isLockedVariable(key)) {
        if (isVariable(key)) {
            variablesTable.deleteSymbol(key);
            return true;
        }
    }
    return false;
}
//=============================================================================
bool
VariablesTable::insertVariable(const key_type& key, const value_type& val)
{
    if (key.empty()) {
        return false;
    }
    if (!IsValidVariableName(key, true)) {
        return false;
    }
    std::lock_guard<std::mutex> lock(accessMutex);
    if (!isLockedVariable(key) || lockedVariables.empty()) {
        variablesTable.insertSymbol(key, val);
        return true;
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getVariablesList(bool withPersistent)
{
    stringVector retlist;
    for (const auto& e : variablesTable.getAllSymbols()) {
        if (withPersistent || (e.empty() || e.at(0) != '_')) {
            retlist.push_back(e);
        }
    }
    return retlist;
}
//=============================================================================
bool
VariablesTable::isLockedVariable(const std::string& key)
{
    return lockedVariables.find(key) != lockedVariables.end();
}
//=============================================================================
bool
VariablesTable::lockVariable(const std::string& key)
{
    if (!isLockedVariable(key)) {
        if (key != "ans") {
            lockedVariables.insert(key);
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
        lockedVariables.erase(key);
        return true;
    }
    return false;
}
//=============================================================================
stringVector
VariablesTable::getLockedVariables()
{
    return stringVector(lockedVariables.begin(), lockedVariables.end());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
