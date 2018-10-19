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
#include "OverloadFunction.hpp"
#include "ClassName.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
namespace Nelson {
ArrayOfVector
OverloadFunction(Evaluator* eval, int nLhs, const ArrayOfVector& argIn,
    const std::string& functionName, bool& bSuccess)
{
    if (functionName.compare("") != 0) {
        if (eval->isOverloadAllowed()) {
            Context* context = eval->getContext();
            FunctionDef* funcDef = nullptr;
            std::string OverloadName = ClassName(argIn[0]) + "_" + functionName;
            if (context->lookupFunction(OverloadName, funcDef)) {
                if ((funcDef->type() == NLS_BUILT_IN_FUNCTION)
                    || (funcDef->type() == NLS_MACRO_FUNCTION)) {
                    bSuccess = true;
                    ArrayOfVector argInCopy = (ArrayOfVector&)argIn;
                    return funcDef->evaluateFunction(eval, argInCopy, nLhs);
                }
            }
        }
    }
    bSuccess = false;
    return ArrayOfVector();
}
//=============================================================================
ArrayOfVector
OverloadFunction(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn, const std::string& functionName)
{
    bool bSuccess;
    ArrayOfVector res = OverloadFunction(eval, nLhs, argIn, functionName, bSuccess);
    if (!bSuccess) {
        OverloadRequired(eval, argIn, Overload::OverloadClass::FUNCTION, functionName);
    }
    return res;
}
//=============================================================================
}
//=============================================================================
