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
#include "FieldCompleter.hpp"
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
FieldCompleter(const std::wstring& prefix)
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
        if (ClassdefDefinitionManager::getInstance()->loadClass(variable)) {
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

        stringVector fielnames;
        if (variableValue.isStruct()) {
            fielnames = variableValue.getFieldNames();
        }
        for (const auto& fieldname : fielnames) {
            if (StringHelpers::starts_with(fieldname, postfix)) {
                res.push_back(utf8_to_wstring(fieldname));
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
