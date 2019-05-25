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
#include "VariableCompleter.hpp"
#include "Evaluator.hpp"
#include "GetNelsonMainEvaluatorDynamicFunction.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
VariableCompleter(const std::wstring& prefix)
{
    wstringVector res;
    auto* eval = static_cast<Evaluator*>(GetNelsonMainEvaluatorDynamicFunction());
    if (eval) {
        Context* context = eval->getContext();
        stringVector variables;
        context->getGlobalScope()->getVariablesList(true, variables);
        stringVector variablesCurrentScope;
        context->getCurrentScope()->getVariablesList(true, variablesCurrentScope);
        variables.insert(
            variables.end(), variablesCurrentScope.begin(), variablesCurrentScope.end());
        stringVector variablesBaseScope;
        context->getBaseScope()->getVariablesList(true, variablesBaseScope);
        variables.insert(variables.end(), variablesBaseScope.begin(), variablesBaseScope.end());
        if (!variables.empty()) {
            std::sort(variables.begin(), variables.end());
            variables.erase(std::unique(variables.begin(), variables.end()), variables.end());
        }
        res.reserve(variables.size());
        for (const auto& variable : variables) {
            res.push_back(utf8_to_wstring(variable));
        }
    }
    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
