//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LocalFunctionsTable.hpp"
#include "BuiltInFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
LocalFunctionsTable::LocalFunctionsTable() { }
//=============================================================================
LocalFunctionsTable::~LocalFunctionsTable() { }
//=============================================================================
bool
LocalFunctionsTable::find(const std::string& key, FunctionDefPtr& dest)
{
    FunctionDefPtr* v = cachedLocalFunctionsTable.findSymbol(key);
    if (v != nullptr) {
        dest = v[0];
        return true;
    }
    return false;
}
//=============================================================================
bool
LocalFunctionsTable::add(const std::string& key, const FunctionDefPtr val)
{
    FunctionDefPtr v = nullptr;
    if (find(key, v)) {
        return false;
    }
    cachedLocalFunctionsTable.insertSymbol(key, val);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
