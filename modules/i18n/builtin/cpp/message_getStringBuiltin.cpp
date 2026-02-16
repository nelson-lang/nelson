//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "message_getStringBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "TranslationManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::I18nGateway::message_getStringBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1);

    ArrayOf msgObj = argIn[0];
    if (!msgObj.isClassType() || msgObj.getClassType() != "message") {
        raiseError2(L"nelson:validators:mustBeType", 1, L"message");
    }
    std::wstring messageId = msgObj.getField("Identifier").getContentAsWideString();
    ArrayOf argumentsArray = msgObj.getField("Arguments");
    std::wstring unformattedString = TranslationManager::getInstance().getError(messageId);
    if (unformattedString == messageId) {
        raiseError2(L"nelson:i18n:messageNotFound", messageId);
    }

    FunctionDefPtr funcDef = nullptr;
    if (!eval->lookupFunction("formatString", funcDef)) {
        raiseError2(L"nelson:runtime:functionNotFound", L"formatString");
    }
    ArrayOfVector inputs;
    inputs << ArrayOf::characterArrayConstructor(unformattedString);
    ArrayOf* arguments = (ArrayOf*)argumentsArray.getDataPointer();
    for (size_t i = 0; i < argumentsArray.getElementCount(); ++i) {
        inputs << arguments[i];
    }
    try {
        ArrayOfVector ouputs = funcDef->evaluateFunction(eval, inputs, 1);
        retval << ouputs[0];
    } catch (const Exception&) {
        raiseError2(L"nelson:runtime:incorrectHoleCount", messageId);
    }
    return retval;
}
//=============================================================================
