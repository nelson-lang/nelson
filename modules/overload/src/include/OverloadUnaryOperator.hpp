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
callOverloadedFunction(Evaluator* eval, ArrayOf a, const std::string& OverloadNameDesired,
    bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    return callOverloadedFunction(
        eval, argsIn, OverloadNameDesired, wasFound, funcDef, bRaiseError);
}
//=============================================================================
static ArrayOf
OverloadUnaryOperator(Evaluator* eval, ArrayOf a, const std::string& functionName, bool bRaiseError,
    bool& bSuccess, std::string forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    std::string classNameA = ClassName(a);
    std::string OverloadName = classNameA + "_" + functionName;
    if (Overload::getPreviousCachedFunctionName(Overload::UNARY) == OverloadName) {
        bSuccess = true;
        return callOverloadedFunction(eval, a,
            Overload::getPreviousCachedFunctionName(Overload::UNARY), bSuccess,
            Overload::getPreviousCachedFunctionDefinition(Overload::UNARY), bRaiseError);
    } else {
        std::string OverloadNameDesired = OverloadName;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::UNARY, OverloadName, funcDef);
            return callOverloadedFunction(
                eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
        }
        if (a.isIntegerType()) {
            OverloadName = NLS_INTEGER_STR + std::string("_") + functionName;
            bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
            if (bSuccess) {
                Overload::setCachedFunction(Overload::UNARY, OverloadName, funcDef);
                return callOverloadedFunction(
                    eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
            }
        }
        OverloadName = NLS_GENERIC_STR + std::string("_") + functionName;
        bSuccess = OverloadFindFunction(eval, OverloadName, &funcDef);
        if (bSuccess) {
            Overload::setCachedFunction(Overload::UNARY, OverloadName, funcDef);
        }
        return callOverloadedFunction(eval, a, OverloadNameDesired, bSuccess, funcDef, bRaiseError);
    }
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(Evaluator* eval, ArrayOf a, const std::string& functionName)
{
    bool bSuccess = false;
    return OverloadUnaryOperator(eval, a, functionName, true, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(Evaluator* eval, ArrayOf a, const std::string& functionName, bool& bSuccess)
{
    return OverloadUnaryOperator(eval, a, functionName, false, bSuccess, "");
}
//=============================================================================
inline ArrayOf
OverloadUnaryOperator(Evaluator* eval, ArrayOf a, const std::string& functionName, bool& bSuccess,
    const std::string& forcedFunctionName)
{
    return OverloadUnaryOperator(eval, a, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
}
//=============================================================================
