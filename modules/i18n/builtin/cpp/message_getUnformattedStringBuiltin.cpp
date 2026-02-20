//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "message_getUnformattedStringBuiltin.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "TranslationManager.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::I18nGateway::message_getUnformattedStringBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 1);

    ArrayOf msgObj = argIn[0];
    if (!msgObj.isClassType() || msgObj.getClassType() != "message") {
        raiseError2(_E("nelson:validators:mustBeTypeAtPosition"), 1, L"message");
    }
    std::wstring messageId = msgObj.getField("Identifier").getContentAsWideString();
    std::wstring unformattedString = TranslationManager::getInstance().getError(messageId);
    if (unformattedString == messageId) {
        raiseError2(_E("nelson:i18n:messageNotFound"), messageId);
    }
    retval << ArrayOf::characterArrayConstructor(unformattedString);
    return retval;
}
//=============================================================================
