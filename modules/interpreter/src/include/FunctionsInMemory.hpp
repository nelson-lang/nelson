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
#include <unordered_map>
#include <map>
#include <vector>
#include "FunctionDef.hpp"
#include "nlsInterpreter_exports.h"
#include "Overload.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
enum OperatorType
{
    COLON_OP = 0,
    UPLUS_OP
};
//=============================================================================
static std::vector<std::string> operatorNames = { "colon", "uplus" };
//=============================================================================
class NLSINTERPRETER_IMPEXP FunctionsInMemory
{
private:
    //=============================================================================
    std::map<std::pair<NelsonType, OperatorType>, FunctionDefPtr> _unaryOperatorInMemory;
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
    add(NelsonType nelsonType, OperatorType operatorType, FunctionDefPtr function);
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
    findUnaryOperator(NelsonType nelsonType, OperatorType operatorType, FunctionDefPtr& function);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
