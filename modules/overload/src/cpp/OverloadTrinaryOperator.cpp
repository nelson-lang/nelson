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
//=============================================================================
namespace Nelson {
    static bool OverloadTrinaryOperatorFindFunction(Evaluator *eval, ArrayOf a, ArrayOf b, ArrayOf c, const std::string &functionName, const std::string &forcedFunctionName, FunctionDef **funcDef, std::string &overloadname)
    {
        bool bSuccess = true;
        Context *context = eval->getContext();
        if (forcedFunctionName.empty())
        {
            overloadname = functionName + "_" + ClassName(a) + "_" + ClassName(b) + "_" + ClassName(c);
        }
        else
        {
            overloadname = forcedFunctionName;
        }
        if (!context->lookupFunction(overloadname, *funcDef))
        {
            bSuccess = false;
        }
        return bSuccess;
    }
    //=============================================================================
    ArrayOf OverloadTrinaryOperator(Evaluator *eval, ArrayOf a, ArrayOf b, ArrayOf c, const std::string &functionName)
    {
        FunctionDef *funcDef = nullptr;
        std::string OverloadName;
        bool bSuccess = OverloadTrinaryOperatorFindFunction(eval, a, b, c, functionName, "", &funcDef, OverloadName);
        std::string forcedName;
        if (!bSuccess)
        {
            forcedName = functionName + ClassName(a) + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
            bSuccess = OverloadTrinaryOperatorFindFunction(eval, a, b, c, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            forcedName = functionName + "_" + NLS_GENERIC_STR + ClassName(b) + NLS_GENERIC_STR;
            bSuccess = OverloadTrinaryOperatorFindFunction(eval, a, b, c, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            forcedName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + ClassName(c);
            bSuccess = OverloadTrinaryOperatorFindFunction(eval, a, b, c, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            forcedName = functionName + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR + "_" + NLS_GENERIC_STR;
            bSuccess = OverloadTrinaryOperatorFindFunction(eval, a, b, c, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            Error(eval, _("function") + " " + OverloadName + " " + _("undefined."));
        }
        ArrayOfVector argsIn;
        argsIn.push_back(a);
        argsIn.push_back(b);
        argsIn.push_back(c);
        int nargout = 1;
        ArrayOfVector res = funcDef->evaluateFunction(eval, argsIn, nargout);
        if (res.size() != 1)
        {
            Error(eval, _("function") + " " + OverloadName + " " + _("only one output argument expected."));
        }
        return res[0];
    }
    //=============================================================================
}
//=============================================================================

