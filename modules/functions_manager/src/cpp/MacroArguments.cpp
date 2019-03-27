//=============================================================================
// Copyright (c) 2016-2019 Allan CORNET (Nelson)
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
MacroArguments(Evaluator* eval, const std::wstring& functionname, wstringVector& Inputs,
    wstringVector& Outputs)
{
    std::string fun = wstring_to_utf8(functionname);
    Context* ctx = eval->getContext();
    FunctionDef* funcDef = nullptr;
    bool isFun = ctx->lookupFunction(fun, funcDef);
    if (isFun) {
        if (funcDef->type() == NLS_MACRO_FUNCTION) {
            auto* valc = (MacroFunctionDef*)funcDef;
            for (const auto& returnVal : valc->returnVals) {
                Inputs.push_back(utf8_to_wstring(returnVal));
            }
            for (const auto& argument : valc->arguments) {
                Outputs.push_back(utf8_to_wstring(argument));
            }
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
