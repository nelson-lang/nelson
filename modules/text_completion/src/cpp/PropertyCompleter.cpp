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
#include "PropertyCompleter.hpp"
#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "characters_encoding.hpp"
#include "StringHelpers.hpp"
#include "EvaluateCommand.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
wstringVector
PropertyCompleter(const std::wstring& prefix)
{
    wstringVector res;
    auto* eval = static_cast<Evaluator*>(NelsonConfiguration::getInstance()->getMainEvaluator());
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
    size_t lastCharacterIndex = variable.find_last_of(" ([{,;");

    if (lastCharacterIndex != std::string::npos) {
        variable = variable.substr(lastCharacterIndex + 1);
    }

    Exception lastError = eval->getLastErrorException();

    try {
        ArrayOfVector variableArray
            = EvaluateCommand(eval, 1, utf8_to_wstring(variable), std::wstring());
        if (variableArray.size() != 1) {
            return res;
        }

        stringVector propertyNames;

        if (variableArray[0].isGraphicsObject() || variableArray[0].isClassType()
            || variableArray[0].isHandle()) {
            ArrayOfVector propertiesArray = EvaluateCommand(
                eval, 1, utf8_to_wstring("properties(" + variable + ")"), std::wstring());
            if (propertiesArray.size() == 1) {
                propertyNames = propertiesArray[0].getContentAsCStringVector();
            }
        }

        for (const auto& name : propertyNames) {
            if (StringHelpers::starts_with(name, postfix)) {
                res.push_back(utf8_to_wstring(name));
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
