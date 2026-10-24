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
#include "PropertyCompleter.hpp"
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
PropertyCompleter(const std::wstring& prefix)
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

    std::string variableExpression = utf8prefix.substr(0, lastDotPos);
    std::string postfix = utf8prefix.substr(lastDotPos + 1);
    std::string variable = getLookupSymbolFromCompletionExpression(variableExpression);

    NelsonException lastError = eval->getLastErrorException();

    try {
        if (variable == variableExpression && classdefLoadClass(variable)) {
            stringVector propertyNames = classdefProperties(variable);
            for (const auto& name : propertyNames) {
                if (StringHelpers::icontains(name, postfix)) {
                    res.push_back(utf8_to_wstring(name));
                }
            }
            return res;
        }

        ArrayOf variableValue;
        if (!resolveCompletionExpressionValue(context, eval, variableExpression, variableValue)) {
            return res;
        }
        if (!isValidCompletionHandle(variableValue)) {
            return res;
        }

        stringVector propertyNames;

        std::string className;
        if (variableValue.isClassType()) {
            className = variableValue.classType();
        } else if (variableValue.isHandle()) {
            className = variableValue.handleClassName();
        }
        if (!className.empty() && classdefLoadClass(className)) {
            propertyNames = classdefProperties(className);
        } else if (variableValue.isGraphicsObject() || variableValue.isHandle()) {
            ArrayOfVector propertiesArray = EvaluateCommand(
                eval, 1, utf8_to_wstring("properties(" + variableExpression + ")"), std::wstring());
            if (propertiesArray.size() == 1) {
                propertyNames = propertiesArray[0].asUtf8StringVector();
            }
        }

        for (const auto& name : propertyNames) {
            if (StringHelpers::icontains(name, postfix)) {
                res.push_back(utf8_to_wstring(name));
            }
        }
    } catch (const NelsonException&) {
        eval->setLastErrorException(lastError);
    }

    return res;
}
//=============================================================================
} // namespace Nelson;
//=============================================================================
