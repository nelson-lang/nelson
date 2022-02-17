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
#include <unordered_map>
#include <vector>
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
#include "Overload.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP FunctionsInMemory
{
private:
    //=============================================================================
    std::vector<std::pair<std::string, FunctionDefPtr>> _macroFunctionsInMemory;
    std::vector<std::pair<std::string, FunctionDefPtr>> _mexFunctionsInMemory;
    std::unordered_map<std::string, FunctionDefPtr> _builtinFunctionInMemory;
    //=============================================================================
    std::unordered_map<std::string, FunctionDefPtr> _lastFunctionsInMemory;
    //=============================================================================
    FunctionsInMemory();
    //=============================================================================
    ~FunctionsInMemory();
    //=============================================================================
    static FunctionsInMemory* m_pInstance;
    //=============================================================================
    bool
    findMex(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
    bool
    findMacro(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
    bool
    findBuiltin(const std::string& functionName, FunctionDefPtr& function);
    //=============================================================================
public:
    //=============================================================================
    enum FIND_FUNCTION_TYPE
    {
        ALL,
        BUILTIN,
        MEX,
        MACRO
    };
    //=============================================================================
    static FunctionsInMemory*
    getInstance();
    //=============================================================================
    void
    destroy();
    //=============================================================================
    void
    add(const std::string& functionName, FunctionDefPtr function);
    //=============================================================================
    void
    add(Overload::OverloadClass overloadClass, const std::string& functionName,
        FunctionDefPtr function);
    //=============================================================================
    bool
    deleteMFunction(const std::string& functionName);
    //=============================================================================
    bool
    deleteMexFunction(const std::string& functionName);
    //=============================================================================
    void
    deleteAllMFunctions();
    //=============================================================================
    bool
    deleteAllMexFunctions();
    //=============================================================================
    bool
    find(const std::string& functionName, FunctionDefPtr& function,
        FIND_FUNCTION_TYPE functionType = FIND_FUNCTION_TYPE::ALL);
    //=============================================================================
    void
    clear(stringVector exceptedFunctions = stringVector());
    //=============================================================================
    void
    clearMapCache();
    //=============================================================================
    wstringVector
    getMacroInMemory(bool withCompleteNames);
    //=============================================================================
    wstringVector
    getMexInMemory(bool withCompleteNames);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
