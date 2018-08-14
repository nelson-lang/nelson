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
#include "OverloadTrinaryOperator.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include <boost/format.hpp>
//=============================================================================
namespace Nelson {
static bool
OverloadTrinaryOperatorFindFunction(
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
buildForcedNameList(const std::string& functionName, ArrayOf a, ArrayOf b, ArrayOf c)
{
    stringVector res;
    std::string classNameA = ClassName(a);
    std::string classNameB = ClassName(b);
    std::string classNameC = ClassName(c);
    boost::format formatFunctionName = boost::format("%s_%s_%s_%s");
    // WARNING: order is important.
    res.push_back(str(formatFunctionName % functionName % classNameA % classNameB % classNameC));
    if (c.isIntegerType()) {
        res.push_back(
            str(formatFunctionName % functionName % classNameA % classNameB % NLS_INTEGER_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % classNameA % classNameB % NLS_GENERIC_STR));
    if (b.isIntegerType()) {
        res.push_back(
            str(formatFunctionName % functionName % classNameA % NLS_INTEGER_STR % classNameC));
    }
    if (b.isIntegerType() && c.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % classNameA % NLS_INTEGER_STR % NLS_INTEGER_STR));
    }
    if (b.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % classNameA % NLS_INTEGER_STR % NLS_GENERIC_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % classNameA % NLS_GENERIC_STR % classNameC));
    if (c.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % classNameA % NLS_GENERIC_STR % NLS_INTEGER_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % classNameA % NLS_GENERIC_STR % NLS_GENERIC_STR));
    if (a.isIntegerType()) {
        res.push_back(
            str(formatFunctionName % functionName % NLS_INTEGER_STR % classNameB % classNameC));
    }
    if (a.isIntegerType() && c.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_INTEGER_STR % classNameB % NLS_INTEGER_STR));
    }
    if (a.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_INTEGER_STR % classNameB % NLS_GENERIC_STR));
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_INTEGER_STR % NLS_INTEGER_STR % classNameC));
    }
    if (a.isIntegerType() && b.isIntegerType() && c.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_INTEGER_STR % NLS_INTEGER_STR
            % NLS_INTEGER_STR));
    }
    if (a.isIntegerType() && b.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_INTEGER_STR % NLS_INTEGER_STR
            % NLS_GENERIC_STR));
    }
    if (a.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_INTEGER_STR % NLS_GENERIC_STR % classNameC));
    }
    if (a.isIntegerType() && c.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_INTEGER_STR % NLS_GENERIC_STR
            % NLS_INTEGER_STR));
    }
    if (a.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_INTEGER_STR % NLS_GENERIC_STR
            % NLS_GENERIC_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % NLS_GENERIC_STR % classNameB % classNameC));
    if (c.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_GENERIC_STR % classNameB % NLS_INTEGER_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % NLS_GENERIC_STR % classNameB % NLS_GENERIC_STR));
    if (b.isIntegerType()) {
        res.push_back(str(
            formatFunctionName % functionName % NLS_GENERIC_STR % NLS_INTEGER_STR % classNameC));
    }
    if (b.isIntegerType() && c.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_GENERIC_STR % NLS_INTEGER_STR
            % NLS_INTEGER_STR));
    }
    if (b.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_GENERIC_STR % NLS_INTEGER_STR
            % NLS_GENERIC_STR));
    }
    res.push_back(
        str(formatFunctionName % functionName % NLS_GENERIC_STR % NLS_GENERIC_STR % classNameC));
    if (c.isIntegerType()) {
        res.push_back(str(formatFunctionName % functionName % NLS_GENERIC_STR % NLS_GENERIC_STR
            % NLS_INTEGER_STR));
    }
    res.push_back(str(
        formatFunctionName % functionName % NLS_GENERIC_STR % NLS_GENERIC_STR % NLS_GENERIC_STR));
    return res;
}
//=============================================================================
ArrayOf
OverloadTrinaryOperator(
    Evaluator* eval, ArrayOf a, ArrayOf b, ArrayOf c, const std::string& functionName)
{
    FunctionDef* funcDef = nullptr;
    stringVector overloadFunctionNameList = buildForcedNameList(functionName, a, b, c);
    std::string OverloadName = overloadFunctionNameList[0];
    bool bSuccess = false;
    for (std::string overloadFunctionName : overloadFunctionNameList) {
        bSuccess = OverloadTrinaryOperatorFindFunction(eval, overloadFunctionName, &funcDef);
        if (bSuccess) {
            break;
        }
    }
    if (!bSuccess) {
        Error(_("function") + " " + OverloadName + " " + _("undefined."));
    }
    ArrayOfVector argsIn;
    argsIn.push_back(a);
    argsIn.push_back(b);
    argsIn.push_back(c);
    int nargout = 1;
    ArrayOfVector res = funcDef->evaluateFunction(eval, argsIn, nargout);
    if (res.size() != 1) {
        Error(
            _("function") + " " + funcDef->name + " " + _("only one output argument expected."));
    }
    return res[0];
}
//=============================================================================
}
//=============================================================================
