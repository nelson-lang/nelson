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
#include "Evaluator.hpp"
#include "Error.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
OverloadFindFunction(Evaluator* eval, const std::string& forcedFunctionName, FunctionDef** funcDef)
{
    Context* context = eval->getContext();
    return context->lookupFunction(forcedFunctionName, *funcDef);
}
//=============================================================================
static ArrayOf
callOverloadedFunction(Evaluator* eval, ArrayOfVector argsIn,
    const std::string& OverloadNameDesired, bool wasFound, FunctionDef* funcDef, bool bRaiseError)
{
    ArrayOf res;
    if (!wasFound) {
        if (bRaiseError) {
            Error(std::string("function ") + OverloadNameDesired + " undefined.");
        } else {
            res = ArrayOf::emptyConstructor();
        }
    } else {
        int nargout = 1;
        ArrayOfVector val = funcDef->evaluateFunction(eval, argsIn, nargout);
        if (val.size() != 1) {
            if (bRaiseError) {
                Error(std::string("function ") + funcDef->name
                    + " only one output argument expected.");
            }
            return ArrayOf::emptyConstructor();
        }
        res = val[0];
    }
    return res;
}
}
//=============================================================================
