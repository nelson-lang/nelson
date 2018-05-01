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
#include "OverloadUnaryOperator.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static bool OverloadUnaryOperatorFindFunction(Evaluator *eval, ArrayOf a, std::string functionName, std::string forcedFunctionName, FunctionDef **funcDef, std::string &overloadname)
    {
        bool bSuccess = true;
        Context *context = eval->getContext();
        if (forcedFunctionName.empty())
        {
            overloadname = ClassName(a) + "_" + functionName;
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
    ArrayOf OverloadUnaryOperator(Evaluator *eval, ArrayOf a, std::string functionName, std::string forcedFunctionName)
    {
        FunctionDef *funcDef = nullptr;
        std::string OverloadName;
        bool bSuccess = OverloadUnaryOperatorFindFunction(eval, a, functionName, forcedFunctionName, &funcDef, OverloadName);
		if (!bSuccess && a.isIntegerType())
		{
			std::string forcedName = NLS_INTEGER_STR + std::string("_") + functionName;
			bSuccess = OverloadUnaryOperatorFindFunction(eval, a, functionName, forcedName, &funcDef, forcedName);
		}
		if (!bSuccess)
        {
            std::string forcedName = NLS_GENERIC_STR + std::string("_") + functionName;
            bSuccess = OverloadUnaryOperatorFindFunction(eval, a, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            Error(eval, _("function") + " " + OverloadName + " " + _("undefined."));
        }
        ArrayOfVector argsIn;
        argsIn.push_back(a);
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
