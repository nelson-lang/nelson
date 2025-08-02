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
#include <unordered_map>
#include <vector>
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSINTERPRETER_IMPEXP FunctionsInMemory
{
private:
    //=============================================================================
    std::unordered_map<std::string, FunctionDefPtr> _macroFunctionsInMemory;
    std::unordered_map<std::string, FunctionDefPtr> _mexFunctionsInMemory;
    std::unordered_map<std::string, FunctionDefPtr> _builtinFunctionInMemory;
    //=============================================================================
    std::unordered_map<std::string, FunctionDefPtr> _lastFunctionsInMemory;
    //=============================================================================
    std::unordered_map<std::string, bool> _notExistingFunctionsInMemory;
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
    void
    declareAsNotExistingFunction(const std::string& functionName);

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
    bool
    isNotExistingFunction(const std::string& functionName);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
