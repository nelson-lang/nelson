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
#include "builtinBuiltin.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::FunctionsGateway::builtinBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    if (argIn.size() < 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    Context* context = eval->getContext();
    FunctionDef* funcDef = nullptr;
    ArrayOf param1 = argIn[0];
    std::string fname = "";
    if (param1.isFunctionHandle()) {
        function_handle fh = param1.getContentAsFunctionHandle();
        std::wstring functionname;
        bool found = PathFuncManager::getInstance()->find(fh, functionname);
        if (!found) {
            found = BuiltInFunctionDefManager::getInstance()->find(fh, functionname);
            if (!found) {
                Error(_W("function handle not defined."));
            }
        }
        fname = wstring_to_utf8(functionname);
    } else {
        fname = argIn[0].getContentAsCString();
    }
    if (!context->lookupFunction(fname, funcDef, true)) {
        Error(_W("function \'") + utf8_to_wstring(fname) + _W("\' is not a builtin."));
    }
    ArrayOfVector newarg(argIn);
    newarg.erase(newarg.begin());
    eval->disableOverload();
    ArrayOfVector retval = funcDef->evaluateFunction(eval, newarg, nLhs);
    eval->enableOverload();
    return retval;
}
//=============================================================================
