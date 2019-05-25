//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
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
#include "StringToFunctionHandle.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
function_handle
StringToFunctionHandle(Evaluator* eval, const std::wstring& functionName)
{
    std::string fun = wstring_to_utf8(functionName);
    Context* ctx = eval->getContext();
    FunctionDef* funcDef = nullptr;
    bool isFun = ctx->lookupFunction(fun, funcDef);
    function_handle functionID = 0;
    if (isFun) {
        functionID = funcDef->hashid;
    }
    return functionID;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
