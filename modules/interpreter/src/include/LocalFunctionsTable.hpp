//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <utility>
#include <string>
#include "FunctionDef.hpp"
#include "GenericTable.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP LocalFunctionsTable
{
private:
    GenericTable<std::string, FunctionDefPtr> cachedLocalFunctionsTable;

public:
    LocalFunctionsTable();
    ~LocalFunctionsTable();
    bool
    find(const std::string& key, FunctionDefPtr& dest);
    bool
    add(const std::string& key, FunctionDefPtr val);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
