//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
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
