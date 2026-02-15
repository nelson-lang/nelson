//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lastwarnBuiltin.hpp"
#include "Error.hpp"
#include "Exception.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lastwarnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    Exception lastWarning = eval->getLastWarningException();
    bool wasReset = false;
    switch (argIn.size()) {
    case 0: {
    } break;
    case 1: {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring message = arg1.getContentAsWideString();
            if (message.empty()) {
                eval->resetLastWarningException();
                wasReset = true;
            } else {
                Exception newLastWarning(message);
                eval->setLastWarningException(newLastWarning);
            }
        } else {
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
        }
    } break;
    case 2: {
        ArrayOf arg1 = argIn[0];
        std::wstring message;
        std::wstring identifier;
        if (arg1.isRowVectorCharacterArray()) {
            message = arg1.getContentAsWideString();
        } else {
            raiseError2(L"nelson:validators:mustBeType", 1, NLS_STRING_ARRAY_STR);
        }
        ArrayOf arg2 = argIn[1];
        if (arg2.isRowVectorCharacterArray()) {
            identifier = arg2.getContentAsWideString();
        } else {
            raiseError2(L"nelson:validators:mustBeType", 2, NLS_STRING_ARRAY_STR);
        }
        Exception newLastWarning(message, identifier);
        eval->setLastWarningException(newLastWarning);
    } break;
    default: {
        raiseError2(L"nelson:arguments:tooManyInputs");
    } break;
    }
    switch (nLhs) {
    case 0: {
        if (!wasReset) {
            std::wstring message = lastWarning.getMessage();
            retval << ArrayOf::characterArrayConstructor(message);
        }
    } break;
    case 1: {
        std::wstring message = lastWarning.getMessage();
        retval << ArrayOf::characterArrayConstructor(message);
    } break;
    case 2: {
        std::wstring message = lastWarning.getMessage();
        std::wstring identifier = lastWarning.getIdentifier();
        retval << ArrayOf::characterArrayConstructor(message);
        retval << ArrayOf::characterArrayConstructor(identifier);
    } break;
    default: {
        raiseError2(L"nelson:arguments:wrongNumberOfOutputs");
    } break;
    }
    return retval;
}
//=============================================================================
