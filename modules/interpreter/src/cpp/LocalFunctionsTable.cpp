//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "LocalFunctionsTable.hpp"
#include "BuiltInFunctionDef.hpp"
#include "GenericTable.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
LocalFunctionsTable::LocalFunctionsTable()
{
    GenericTable<FunctionDefPtr>* genericTable = nullptr;
    try {
        genericTable = new GenericTable<FunctionDefPtr>;
    } catch (const std::bad_alloc&) {
        genericTable = nullptr;
    }
    cachedLocalFunctionsTable = (void*)genericTable;
}
//=============================================================================
LocalFunctionsTable::~LocalFunctionsTable()
{
    if (cachedLocalFunctionsTable != nullptr) {
        auto* genericTable = (GenericTable<FunctionDefPtr>*)cachedLocalFunctionsTable;
        delete genericTable;
        genericTable = nullptr;
    }
}
//=============================================================================
bool
LocalFunctionsTable::find(const std::string& key, FunctionDefPtr& dest)
{
    if (cachedLocalFunctionsTable != nullptr) {
        auto* genericTable = (GenericTable<FunctionDefPtr>*)cachedLocalFunctionsTable;
        FunctionDefPtr* v = genericTable->findSymbol(key);
        if (v != nullptr) {
            dest = v[0];
            return true;
        }
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
    if (cachedLocalFunctionsTable != nullptr) {
        auto* genericTable = (GenericTable<FunctionDefPtr>*)cachedLocalFunctionsTable;
        genericTable->insertSymbol(key, val);
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
