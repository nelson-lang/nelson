//=============================================================================
// Copyright (c) 2016-2017 Allan CORNET (Nelson)
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
#include "Exception.hpp"
//=============================================================================
namespace Nelson {
    //=============================================================================
    static bool OverloadBinaryOperatorFindFunction(Evaluator *eval, ArrayOf a, ArrayOf b, std::string functionName, std::string forcedFunctionName, FunctionDef **funcDef, std::string &overloadname)
    {
        bool bSuccess = true;
        Context *context = eval->getContext();
        if (forcedFunctionName.empty())
        {
            overloadname = ClassName(a) + "_" + functionName + "_" + ClassName(b);
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
    static ArrayOf OverloadBinaryOperator(Evaluator *eval, ArrayOf a, ArrayOf b, std::string functionName, bool bRaiseError, bool &bSuccess, std::string forcedFunctionName)
    {
        FunctionDef *funcDef = nullptr;
        std::string forcedName;
        std::string OverloadName;
        bSuccess = OverloadBinaryOperatorFindFunction(eval, a, b, functionName, forcedFunctionName, &funcDef, OverloadName);
        if (!bSuccess)
        {
            forcedName = ClassName(a) + std::string("_") + functionName + std::string("_") + NLS_GENERIC_STR;
            bSuccess = OverloadBinaryOperatorFindFunction(eval, a, b, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            forcedName = NLS_GENERIC_STR + std::string("_") + functionName + std::string("_") + ClassName(b);
            bSuccess = OverloadBinaryOperatorFindFunction(eval, a, b, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            forcedName = NLS_GENERIC_STR + std::string("_") + functionName + std::string("_") + NLS_GENERIC_STR;
            bSuccess = OverloadBinaryOperatorFindFunction(eval, a, b, functionName, forcedName, &funcDef, forcedName);
        }
        if (!bSuccess)
        {
            if (bRaiseError)
            {
                throw Exception(std::string("function ") + OverloadName + " undefined.");
            }
            return ArrayOf::emptyConstructor();
        }
        ArrayOfVector argsIn;
        argsIn.push_back(a);
        argsIn.push_back(b);
        int nargout = 1;
        ArrayOfVector res = funcDef->evaluateFunction(eval, argsIn, nargout);
        if (res.size() != 1)
        {
            if (bRaiseError)
            {
                throw Exception(std::string("function ") + OverloadName + " only one output argument expected.");
            }
            bSuccess = false;
            return ArrayOf::emptyConstructor();
        }
        return res[0];
    }
    //=============================================================================
    inline ArrayOf OverloadBinaryOperator(Evaluator *eval, ArrayOf a, ArrayOf b, std::string functionName)
    {
        bool bSuccess = false;
        return OverloadBinaryOperator(eval, a, b, functionName, true, bSuccess, std::string());
    }
    //=============================================================================
    inline ArrayOf OverloadBinaryOperator(Evaluator *eval, ArrayOf a, ArrayOf b, std::string functionName, bool &bSuccess)
    {
        return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, std::string());
    }
    //=============================================================================
    inline ArrayOf OverloadBinaryOperator(Evaluator *eval, ArrayOf a, ArrayOf b, std::string functionName, bool &bSuccess, std::string forcedFunctionName)
    {
        return OverloadBinaryOperator(eval, a, b, functionName, false, bSuccess, forcedFunctionName);
    }
    //=============================================================================
}
//=============================================================================
