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
#include "MacroArguments.hpp"
#include "MacroFunctionDef.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
MacroArguments(
    Evaluator* eval, std::wstring functionname, wstringVector& Inputs, wstringVector& Outputs)
{
    std::string fun = wstring_to_utf8(functionname);
    Context* ctx = eval->getContext();
    FunctionDef* funcDef = nullptr;
    bool isFun = ctx->lookupFunction(fun, funcDef);
    if (isFun) {
        if (funcDef->type() == NLS_MACRO_FUNCTION) {
            MacroFunctionDef* valc = (MacroFunctionDef*)funcDef;
            for (size_t k = 0; k < valc->returnVals.size(); k++) {
                Inputs.push_back(utf8_to_wstring(valc->returnVals[k]));
            }
            for (size_t k = 0; k < valc->arguments.size(); k++) {
                Outputs.push_back(utf8_to_wstring(valc->arguments[k]));
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
}
//=============================================================================
