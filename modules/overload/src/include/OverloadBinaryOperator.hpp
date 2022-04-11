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
callOverloadedFunction(Evaluator* eval, const ArrayOf& a, const ArrayOf& b,
    const std::string& OverloadNameDesired, bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    argsIn.push_back(b);
    return callOverloadedFunction(
        eval, argsIn, OverloadNameDesired, wasFound, funcDef, bRaiseError);
}
//=============================================================================
static ArrayOf
OverloadBinaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b,
    const std::string& functionName, bool bRaiseError, bool& bSuccess,
    const std::string& forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    std::string classNameA = ClassName(a);
    std::string classNameB = ClassName(b);
    // WARNING: order is important.
    std::string OverloadName = classNameA + "_" + functionName + "_" + classNameB;
    std::string OverloadNameDesired = OverloadName;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (a.isIntegerType()) {
        OverloadName = std::string(NLS_INTEGER_STR) + "_" + functionName + "_" + classNameB;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (b.isIntegerType()) {
        OverloadName = classNameA + "_" + functionName + "_" + std::string(NLS_INTEGER_STR);
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        OverloadName = std::string(NLS_INTEGER_STR) + "_" + functionName + "_"
            + std::string(NLS_INTEGER_STR);
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = classNameA + "_" + functionName + "_" + std::string(NLS_GENERIC_STR);
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (a.isIntegerType()) {
        OverloadName = std::string(NLS_INTEGER_STR) + "_" + functionName + "_"
            + std::string(NLS_GENERIC_STR);
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = std::string(NLS_GENERIC_STR) + "_" + functionName + "_" + classNameB;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (a.isIntegerType()) {
        OverloadName = std::string(NLS_GENERIC_STR) + "_" + functionName + "_"
            + std::string(NLS_INTEGER_STR);
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName
        = std::string(NLS_GENERIC_STR) + "_" + functionName + "_" + std::string(NLS_GENERIC_STR);
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    return callOverloadedFunction(eval, a, b, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(
    Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const std::string& functionName)
{
    bool bSuccess = false;
    return OverloadBinaryOperator(eval, a, b, functionName, true, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b,
    const std::string& functionName, bool& bSuccess)
{
    return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b,
    const std::string& functionName, bool& bSuccess, const std::string& forcedFunctionName)
{
    return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
