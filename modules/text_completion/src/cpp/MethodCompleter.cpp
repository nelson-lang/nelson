//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include "MethodCompleter.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "EvaluateCommand.hpp"
#include "ClassdefParser.hpp"
#include "CompleterHelper.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
MethodCompleter(const std::wstring& prefix)
{
    wstringVector res;
    auto* eval = getCompletionEvaluator();
    if (!eval) {
        return res;
    }
    Context* context = eval->getContext();
    std::string utf8prefix = wstring_to_utf8(prefix);
    size_t lastDotPos = utf8prefix.rfind('.');

    if (lastDotPos == std::string::npos) {
        return res;
    }

    std::string variable = utf8prefix.substr(0, lastDotPos);
    std::string postfix = utf8prefix.substr(lastDotPos + 1);
    variable = getLookupSymbolFromCompletionExpression(variable);

    Exception lastError = eval->getLastErrorException();

    try {
        auto* classdefManager = ClassdefDefinitionManager::getInstance();
        if (classdefManager->loadClass(variable)) {
            stringVector methodNames = classdefManager->methods(variable);
            for (const auto& name : methodNames) {
                if (name != variable && StringHelpers::starts_with(name, postfix)) {
                    res.push_back(utf8_to_wstring(name));
                }
            }
            return res;
        }

        ArrayOf variableValue;
        if (!lookupCompletionVariable(context, variable, variableValue)) {
            ArrayOfVector variableArray
                = EvaluateCommand(eval, 1, utf8_to_wstring(variable), std::wstring());
            if (variableArray.size() != 1) {
                return res;
            }
            variableValue = variableArray[0];
        }

        stringVector methodNames;
        std::string className;
        if (variableValue.isClassType()) {
            className = variableValue.getClassType();
        } else if (variableValue.isHandle()) {
            className = variableValue.getHandleClassName();
        }
        if (!className.empty() && classdefManager->loadClass(className)) {
            methodNames = classdefManager->methods(className);
        } else if (variableValue.isGraphicsObject() || variableValue.isHandle()) {
            ArrayOfVector methodsArray = EvaluateCommand(
                eval, 1, utf8_to_wstring("methods(" + variable + ")"), std::wstring());
            if (methodsArray.size() == 1) {
                methodNames = methodsArray[0].getContentAsCStringVector();
            }
            if (variableValue.isHandle()) {
                className = variableValue.getHandleCategory();
            }
        }

        for (const auto& name : methodNames) {
            if (StringHelpers::starts_with(name, postfix)) {
                if (name != className) {
                    res.push_back(utf8_to_wstring(name));
                }
            }
        }
    } catch (const Exception&) {
        eval->setLastErrorException(lastError);
    }

    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
