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
#include "function_handle_extractionBuiltin.hpp"
#include "ArrayOf.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionHandleGateway::function_handle_extractionBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 0) {
        ArrayOf Arg1 = argIn[0];
        if (Arg1.isFunctionHandle()) {
            function_handle fh = Arg1.getContentAsFunctionHandle();
            FunctionDef* funcDef = nullptr;
            std::wstring functionName;
            bool found = PathFuncManager::getInstance()->find(fh, functionName);
            if (found) {
                PathFuncManager::getInstance()->find(wstring_to_utf8(functionName), funcDef);
            } else {
                found = BuiltInFunctionDefManager::getInstance()->find(fh, functionName);
                if (found) {
                    BuiltInFunctionDefManager::getInstance()->find(
                        wstring_to_utf8(functionName), funcDef);
                }
            }
            if (funcDef != nullptr) {
                ArrayOfVector m;
                for (size_t k = 1; k < argIn.size(); k++) {
                    m.push_back(argIn[k]);
                }
                retval = funcDef->evaluateFunction(eval, m, nLhs);
            } else {
                Error(_W("Function does not exist."));
            }
        } else {
            Error(_W("Argument #1 must be a valid function_handle."));
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
