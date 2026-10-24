//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CallbackBase.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
CallbackBase::executeCallbackImpl(Evaluator* eval)
{
    running = true;

    if (this->callbackAsArrayOf.isEmpty()) {
        running = false;
        return false;
    }

    if (callbackAsArrayOf.isCell()) {
        size_t nbElements = callbackAsArrayOf.elementCount();
        const ArrayOf* elements = (const ArrayOf*)callbackAsArrayOf.dataPointer();
        const ArrayOf& fhArrayOf = elements[0];
        if (!fhArrayOf.isFunctionHandle()) {
            running = false;
            return false;
        }
        ArrayOfVector argIn;
        for (size_t k = 1; k < nbElements; ++k) {
            argIn.push_back(elements[k]);
        }
        function_handle fh = fhArrayOf.asFunctionHandle();
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        try {
            anonymousFunction->evaluateFunction(eval, argIn, 0);
        } catch (const NelsonException& e) {
            eval->getInterface()->errorMessage(e.formattedErrorMessage());
            eval->setLastErrorException(e);
            running = false;
            return false;
        }
        running = false;
        return true;
    } else if (callbackAsArrayOf.isRowVectorCharacterArray()
        || callbackAsArrayOf.isScalarStringArray()) {
        std::unique_ptr<wchar_t[]> buffer(callbackAsArrayOf.toAllocatedWideCString());
        std::wstring callbackString = buffer.get();
        callbackString += L"\n";
        try {
            bool result = eval->evaluateString(callbackString, true);
            running = false;
            return result;
        } catch (const NelsonException& e) {
            eval->getInterface()->errorMessage(e.formattedErrorMessage());
            eval->setLastErrorException(e);
            running = false;
            return false;
        }
    }
    running = false;
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
