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
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
callOverloadedFunction(Evaluator* eval, const ArrayOf& a, const std::string& OverloadNameDesired,
    bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    return callOverloadedFunction(
        eval, argsIn, OverloadNameDesired, wasFound, funcDef, bRaiseError);
}
//=============================================================================
static ArrayOf
OverloadUnaryOperator(Evaluator* eval, const ArrayOf& a, const std::string& functionName,
    bool bRaiseError, bool& bSuccess, const std::string& forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    std::string classNameA = ClassName(a);
    std::string OverloadName = classNameA + "_" + functionName;
    std::string OverloadNameDesired = OverloadName;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (a.isIntegerType()) {
        OverloadName = NLS_INTEGER_STR + std::string("_") + functionName;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = NLS_GENERIC_STR + std::string("_") + functionName;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    return callOverloadedFunction(eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(Evaluator* eval, const ArrayOf& a, const std::string& functionName)
{
    bool bSuccess = false;
    return OverloadUnaryOperator(eval, a, functionName, true, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(
    Evaluator* eval, const ArrayOf& a, const std::string& functionName, bool& bSuccess)
{
    return OverloadUnaryOperator(eval, a, functionName, false, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(Evaluator* eval, const ArrayOf& a, const std::string& functionName,
    bool& bSuccess, const std::string& forcedFunctionName)
{
    return OverloadUnaryOperator(eval, a, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
