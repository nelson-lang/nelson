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
#pragma once
//=============================================================================
#include <vector>
#include <unordered_map>
#include <string>
#include "BuiltInFunctionDef.hpp"
#include "NelsonGateway.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
class NLSINTERPRETER_IMPEXP BuiltInFunctionDefManager
{
public:
    static BuiltInFunctionDefManager*
    getInstance();
    bool
    add(const std::string& name, void* fptr, int argc_in, int argc_out,
        const std::wstring& dynlibname, const std::wstring& modulename, size_t builtinPrototype,
        bool interleavedComplex);

    bool
    remove(const std::string& name);
    bool
    remove(FunctionDefPtr ptr);
    bool
    remove(BuiltInFunctionDef* ptr);
    bool
    remove(void* fptr);
    bool
    removeAll();

    void
    destroy();

    std::vector<FunctionDefPtr>
    getTable();
    stringVector
    getNameList();
    bool
    find(const std::string& name, FunctionDefPtr& ptr);
    bool
    find(const std::string& name, wstringVector& paths);
    bool
    find(const std::string& name, std::wstring& path);

private:
    bool
    add(FunctionDefPtr ptr);

    BuiltInFunctionDefManager();
    static BuiltInFunctionDefManager* m_pInstance;
    std::vector<FunctionDefPtr> builtinVector;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
