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
#include "ArrayOf.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "Evaluator.hpp"
#include <boost/format.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
OverloadBinaryOperatorFindFunction(
    Evaluator* eval, const std::string& forcedFunctionName, FunctionDef** funcDef)
{
    bool bSuccess = true;
    Context* context = eval->getContext();
    if (!context->lookupFunction(forcedFunctionName, *funcDef)) {
        bSuccess = false;
    }
    return bSuccess;
}
//=============================================================================
static stringVector
buildForcedNameList(const std::string& functionName, ArrayOf a, ArrayOf b)
{
    stringVector res;
    std::string classNameA = ClassName(a);
    std::string classNameB = ClassName(b);
    boost::format formatFunctionName = boost::format("%s_%s_%s");
    // WARNING: order is important.
    res.push_back(str(formatFunctionName % classNameA % functionName % classNameB));
    if (a.isIntegerType()) {
        res.push_back(str(formatFunctionName % NLS_INTEGER_STR % functionName % classNameB));
    }
    if (b.isIntegerType()) {
        res.push_back(str(formatFunctionName % classNameA % functionName % NLS_INTEGER_STR));
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        res.push_back(str(formatFunctionName % NLS_INTEGER_STR % functionName % NLS_INTEGER_STR));
    }
    res.push_back(str(formatFunctionName % classNameA % functionName % NLS_GENERIC_STR));
    if (a.isIntegerType()) {
        res.push_back(str(formatFunctionName % NLS_INTEGER_STR % functionName % NLS_GENERIC_STR));
    }
    res.push_back(str(formatFunctionName % NLS_GENERIC_STR % functionName % classNameB));
    if (a.isIntegerType()) {
        res.push_back(str(formatFunctionName % NLS_GENERIC_STR % functionName % NLS_INTEGER_STR));
    }
    res.push_back(str(formatFunctionName % NLS_GENERIC_STR % functionName % NLS_GENERIC_STR));
    return res;
}
//=============================================================================
static ArrayOf
OverloadBinaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, std::string functionName,
    bool bRaiseError, bool& bSuccess, std::string forcedFunctionName)
{
    FunctionDef* funcDef = nullptr;
    stringVector overloadFunctionNameList = buildForcedNameList(functionName, a, b);
    std::string OverloadName = overloadFunctionNameList[0];
    bSuccess = false;
    for (std::string overloadFunctionName : overloadFunctionNameList) {
        bSuccess = OverloadBinaryOperatorFindFunction(eval, overloadFunctionName, &funcDef);
        if (bSuccess) {
            break;
        }
    }
    if (!bSuccess) {
        if (bRaiseError) {
            Error(eval, std::string("function ") + OverloadName + " undefined.");
        }
        return ArrayOf::emptyConstructor();
    }
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    argsIn.push_back(b);
    int nargout = 1;
    ArrayOfVector res = funcDef->evaluateFunction(eval, argsIn, nargout);
    if (res.size() != 1) {
        if (bRaiseError) {
            Error(eval,
                std::string("function ") + funcDef->name + " only one output argument expected.");
        }
        bSuccess = false;
        return ArrayOf::emptyConstructor();
    }
    return res[0];
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, std::string functionName)
{
    bool bSuccess = false;
    return OverloadBinaryOperator(eval, a, b, functionName, true, bSuccess, std::string());
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(
    Evaluator* eval, ArrayOf a, ArrayOf b, std::string functionName, bool& bSuccess)
{
    return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, std::string());
}
//=============================================================================
inline ArrayOf
OverloadBinaryOperator(Evaluator* eval, ArrayOf a, ArrayOf b, std::string functionName,
    bool& bSuccess, std::string forcedFunctionName)
{
    return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, forcedFunctionName);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
