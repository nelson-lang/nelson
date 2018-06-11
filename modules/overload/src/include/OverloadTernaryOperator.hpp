//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "Evaluator.hpp"
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "OverloadCache.hpp"
#include "OverloadHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
callOverloadedFunction(Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c,
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
OverloadTernaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c,
    const std::string& functionName, bool bRaiseError, bool& bSuccess,
    std::string forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    std::string classNameA = ClassName(a);
    std::string classNameB = ClassName(b);
    std::string classNameC = ClassName(c);
    // WARNING: order is important.
    std::string OverloadName
        = functionName + "_" + classNameA + "_" + classNameB + "_" + classNameC;
    if (Overload::getPreviousCachedFunctionName(Overload::TERNARY) == OverloadName) {
        bSuccess = true;
        return callOverloadedFunction(eval, a, b, c,
            Overload::getPreviousCachedFunctionName(Overload::TERNARY), bSuccess,
            Overload::getPreviousCachedFunctionDefinition(Overload::TERNARY), bRaiseError);
    } else {
        std::string OverloadNameDesired = OverloadName;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (c.isIntegerType()) {
            OverloadName
                = functionName + "_" + classNameA + "_" + classNameB + "_" + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName = functionName + "_" + classNameA + "_" + classNameB + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (b.isIntegerType()) {
            OverloadName
                = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + classNameC;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (b.isIntegerType() && c.isIntegerType()) {
            OverloadName
                = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (b.isIntegerType()) {
            OverloadName
                = functionName + "_" + classNameA + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (c.isIntegerType()) {
            OverloadName
                = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName
            = functionName + "_" + classNameA + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (a.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + classNameC;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType() && c.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_INTEGER_STR + "_" + classNameB + "_" + NLS_GENERIC_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType() && b.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_" + classNameC;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType() && b.isIntegerType() && c.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_"
                + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType() && b.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_INTEGER_STR + "_"
                + NLS_GENERIC_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_" + classNameC;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType() && c.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_"
                + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (a.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_INTEGER_STR + "_" + NLS_GENERIC_STR + "_"
                + NLS_GENERIC_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (c.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + classNameB + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (b.isIntegerType()) {
            OverloadName
                = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_" + classNameC;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (b.isIntegerType() && c.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_"
                + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        if (b.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_INTEGER_STR + "_"
                + NLS_GENERIC_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + classNameC;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (c.isIntegerType()) {
            OverloadName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_"
                + NLS_INTEGER_STR;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName
            = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::TERNARY, OverloadName, funcDef);
        }
        return callOverloadedFunction(
            eval, a, b, c, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(
    Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c, const std::string& functionName)
{
    bool bSuccess = false;
    return OverloadTernaryOperator(eval, a, b, c, functionName, true, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c,
    const std::string& functionName, bool& bSuccess)
{
    return OverloadTernaryOperator(eval, a, b, c, functionName, false, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadTernaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c,
    const std::string& functionName, bool& bSuccess, const std::string& forcedFunctionName)
{
    return OverloadTernaryOperator(
        eval, a, b, c, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
}
//=============================================================================
