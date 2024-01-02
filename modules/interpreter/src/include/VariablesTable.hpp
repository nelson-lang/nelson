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
#include <string>
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsInterpreter_exports.h"
#include "GenericTable.hpp"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP VariablesTable
{
    //=============================================================================
private:
    using key_type = std::string;
    using value_type = ArrayOf;
    stringVector lockedVariables;
    GenericTable<std::string, ArrayOf> variablesTable;
    volatile bool lockedAccess = false;
    //=============================================================================
public:
    VariablesTable();
    ~VariablesTable();
    bool
    findVariable(const key_type& key, value_type& dest);
    ArrayOf*
    findVariable(const std::string& key);
    bool
    isVariable(const key_type& key);
    bool
    deleteVariable(const key_type& key);
    bool
    insertVariable(const key_type& key, const value_type& val);
    stringVector
    getVariablesList(bool withPersistent);
    bool
    isLockedVariable(const std::string& key);
    bool
    lockVariable(const std::string& key);
    bool
    unlockVariable(const std::string& key);
    stringVector
    getLockedVariables();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
