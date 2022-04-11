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
callOverloadedFunction(Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c,
    const std::string& OverloadNameDesired, bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    argsIn.push_back(b);
    argsIn.push_back(c);
    return callOverloadedFunction(
        eval, argsIn, OverloadNameDesired, wasFound, funcDef, bRaiseError);
}
//=============================================================================
static ArrayOf
OverloadTernaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c,
    const std::string& functionName, bool bRaiseError, bool& bSuccess,
    const std::string& forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    std::string classNameA = ClassName(a);
    std::string classNameB = ClassName(b);
    std::string classNameC = ClassName(c);
    // WARNING: order is important.
    std::string OverloadName
        = functionName + "_" + classNameA + "_" + classNameB + "_" + classNameC;
    std::string OverloadNameDesired = OverloadName;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (c.isIntegerType()) {
        OverloadName = functionName + "_" + classNameA + "_" + classNameB + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + classNameA + "_" + classNameB + "_" + NLS_GENERIC_STR;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (b.isIntegerType()) {
        OverloadName = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (b.isIntegerType() && c.isIntegerType()) {
        OverloadName
            = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (b.isIntegerType()) {
        OverloadName
            = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + classNameC;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (c.isIntegerType()) {
        OverloadName
            = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (a.isIntegerType()) {
        OverloadName = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && b.isIntegerType() && c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType() && c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (a.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + classNameC;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + NLS_GENERIC_STR;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (b.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (b.isIntegerType() && c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    if (b.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + classNameC;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    if (bSuccess) {
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
    if (c.isIntegerType()) {
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
    }
    OverloadName
        = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
    bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
    return callOverloadedFunction(
        eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c,
    const std::string& functionName)
{
    bool bSuccess = false;
    return OverloadTernaryOperator(eval, a, b, c, functionName, true, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c,
    const std::string& functionName, bool& bSuccess)
{
    return OverloadTernaryOperator(eval, a, b, c, functionName, false, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(Evaluator* eval, const ArrayOf& a, const ArrayOf& b, const ArrayOf& c,
    const std::string& functionName, bool& bSuccess, const std::string& forcedFunctionName)
{
    return OverloadTernaryOperator(
        eval, a, b, c, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
