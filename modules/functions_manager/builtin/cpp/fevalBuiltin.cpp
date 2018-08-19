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
#include "fevalBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::fevalBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() < 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    std::string fname;
    ArrayOf param1 = argIn[0];
    if (param1.isFunctionHandle()) {
        std::wstring functionname;
        function_handle fh = param1.getContentAsFunctionHandle();
        bool found = PathFuncManager::getInstance()->find(fh, functionname);
        if (!found) {
            found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
        }
        if (found) {
            fname = wstring_to_utf8(functionname);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED);
        }
    } else {
        fname = param1.getContentAsCString();
    }
    if (!context->lookupFunction(fname, funcDef)) {
        Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a function."));
    }
    ArrayOfVector newarg(argIn);
    newarg.erase(newarg.begin());
    eval->disableOverload();
    ArrayOfVector retval = funcDef->evaluateFunction(eval, newarg, nLhs);
    eval->enableOverload();
    return retval;
}
//=============================================================================
