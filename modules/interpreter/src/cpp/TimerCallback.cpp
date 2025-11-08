//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "TimerCallback.hpp"
#include "AnonymousMacroFunctionDef.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
TimerCallback::execute(Evaluator* eval)
{
    running = true;

    if (this->callbackAsArrayOf.isEmpty()) {
        running = false;
        return false;
    }

    auto executeCallback = [&](const std::function<void()>& callback) {
        try {
            callback();
            running = false;
            return true;
        } catch (const Exception& e) {
            eval->setLastErrorException(e);
            eval->getInterface()->errorMessage(e.getFormattedErrorMessage());
            running = false;
            return false;
        }
    };

    if (callbackAsArrayOf.isCell()) {
        size_t nbElements = callbackAsArrayOf.getElementCount();
        const ArrayOf* elements = (const ArrayOf*)callbackAsArrayOf.getDataPointer();
        const ArrayOf& fhArrayOf = elements[0];
        if (!fhArrayOf.isFunctionHandle()) {
            running = false;
            return false;
        }
        ArrayOfVector argIn;
        for (size_t k = 1; k < nbElements; ++k) {
            argIn.push_back(elements[k]);
        }
        function_handle fh = fhArrayOf.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        try {
            anonymousFunction->evaluateFunction(eval, argIn, 0);
        } catch (const Exception& e) {
            eval->getInterface()->errorMessage(e.getFormattedErrorMessage());
            eval->setLastErrorException(e);
            return false;
        }
        return true;
    } else if (callbackAsArrayOf.isRowVectorCharacterArray()
        || callbackAsArrayOf.isScalarStringArray()) {
        std::wstring callbackString = callbackAsArrayOf.getContentAsWideCharactersPointer();
        callbackString += L"\n";
        try {
            return eval->evaluateString(callbackString, true);
        } catch (const Exception& e) {
            eval->getInterface()->errorMessage(e.getFormattedErrorMessage());
            eval->setLastErrorException(e);
            return false;
        }
    }
    running = false;
    return false;
}
//=============================================================================
}
//=============================================================================
