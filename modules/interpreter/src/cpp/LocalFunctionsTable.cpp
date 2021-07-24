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
