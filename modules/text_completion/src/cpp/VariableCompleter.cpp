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
#include "VariableCompleter.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
VariableCompleter(std::wstring prefix)
{
    wstringVector res;
    Evaluator* eval = (Evaluator*)GetNelsonMainEvaluatorDynamicFunction();
    if (eval) {
        stringVector variables = eval->getContext()->getGlobalScope()->getVariablesList(true);
        stringVector variablesCurrentScope
            = eval->getContext()->getCurrentScope()->getVariablesList(true);
        variables.insert(
            variables.end(), variablesCurrentScope.begin(), variablesCurrentScope.end());
        stringVector variablesBaseScope
            = eval->getContext()->getBaseScope()->getVariablesList(true);
        variables.insert(variables.end(), variablesBaseScope.begin(), variablesBaseScope.end());
        std::sort(variables.begin(), variables.end());
        variables.erase(std::unique(variables.begin(), variables.end()), variables.end());
        res.reserve(variables.size());
        for (size_t k = 0; k < variables.size(); k++) {
            res.push_back(utf8_to_wstring(variables[k]));
        }
    }
    return res;
}
//=============================================================================
};
//=============================================================================
