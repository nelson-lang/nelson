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
    for (size_t k = 0; k < funcs.size(); k++) {
        if (bWithPrivateFunction) {
            functionsList.push_back(utf8_to_wstring(funcs[k]));
        } else {
            if (funcs[k][0] != '_') {
                functionsList.push_back(utf8_to_wstring(funcs[k]));
            }
        }
    }
    if (bSorted) {
        std::sort(functionsList.begin(), functionsList.end());
    }
    return functionsList;
}
//=============================================================================
wstringVector
WhatListOfBuiltin(bool bSorted)
{
    wstringVector functionsList;
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
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
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        macroList = WhatListOfMacro(eval);
    }
    return macroList;
}
//=============================================================================
}
//=============================================================================
