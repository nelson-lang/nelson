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
#include "ErrorID.hpp"
#include "NelsonException.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lastwarnBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    NelsonException lastWarning = eval->getLastWarningException();
    bool wasReset = false;
    switch (argIn.size()) {
    case 0: {
    } break;
    case 1: {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring message = arg1.asWideString();
            if (message.empty()) {
                eval->resetLastWarningException();
                wasReset = true;
            } else {
                NelsonException newLastWarning(message);
                eval->setLastWarningException(newLastWarning);
            }
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } break;
    case 2: {
        ArrayOf arg1 = argIn[0];
        std::wstring message;
        std::wstring identifier;
        if (arg1.isRowVectorCharacterArray()) {
            message = arg1.asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
        ArrayOf arg2 = argIn[1];
        if (arg2.isRowVectorCharacterArray()) {
            identifier = arg2.asWideString();
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2),
                L"string");
        }
        NelsonException newLastWarning(message, identifier);
        eval->setLastWarningException(newLastWarning);
    } break;
    default: {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    } break;
    }
    switch (nLhs) {
    case 0: {
        if (!wasReset) {
            std::wstring message = lastWarning.message();
            retval << ArrayOf::characterArray(message);
        }
    } break;
    case 1: {
        std::wstring message = lastWarning.message();
        retval << ArrayOf::characterArray(message);
    } break;
    case 2: {
        std::wstring message = lastWarning.message();
        std::wstring identifier = lastWarning.identifier();
        retval << ArrayOf::characterArray(message);
        retval << ArrayOf::characterArray(identifier);
    } break;
    default: {
        Error(L"Nelson:tooManyOutputs", L"Too many output arguments.");
    } break;
    }
    return retval;
}
//=============================================================================
