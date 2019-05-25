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
#include "What.hpp"
#include "BuiltInFunctionDefManager.hpp"
#include "Error.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "PathFuncManager.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
WhatListOfBuiltin(Evaluator* eval, bool bWithPrivateFunction, bool bSorted)
{
    wstringVector functionsList;
    stringVector funcs = eval->getContext()->getGlobalScope()->getBuiltinsList();
    BuiltInFunctionDefManager::getInstance()->getNameList();
    for (auto& func : funcs) {
        if (bWithPrivateFunction) {
            functionsList.push_back(utf8_to_wstring(func));
        } else {
            if (func[0] != '_') {
                functionsList.push_back(utf8_to_wstring(func));
            }
        }
    }
    if (bSorted) {
        if (!functionsList.empty()) {
            std::sort(functionsList.begin(), functionsList.end());
        }
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfBuiltin(bool bSorted)
{
    wstringVector functionsList;
    auto* eval = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    if (eval != nullptr) {
        functionsList = WhatListOfBuiltin(eval, bSorted);
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfMacro(Evaluator* eval)
{
    return PathFuncManager::getInstance()->getMacrosList();
}
//=============================================================================
wstringVector
WhatListOfMacro()
{
    wstringVector macroList;
    auto* eval = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    if (eval != nullptr) {
        macroList = WhatListOfMacro(eval);
    }
    return macroList;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
