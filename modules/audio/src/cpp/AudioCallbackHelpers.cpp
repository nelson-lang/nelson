//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AudioCallbackHelpers.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "EventCallback.hpp"
#include "EventQueue.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::wstring
eventLoopRequiredMessage(const std::wstring& name);
//=============================================================================
bool
isValidAudioCallback(const ArrayOf& callback)
{
    return callback.isScalarStringArray() || callback.isRowVectorCharacterArray()
        || callback.isFunctionHandle();
}
//=============================================================================
bool
setAudioCallback(const std::wstring& name, const ArrayOf& value, bool withEventLoop,
    ArrayOf& callback, std::wstring& errorMessage)
{
    if (!withEventLoop) {
        errorMessage = eventLoopRequiredMessage(name);
        return false;
    }
    if (!isValidAudioCallback(value)) {
        errorMessage = formatI18nMessage(resolveMessageTemplate(L"Nelson:audio:functionHandle",
                                             L"%s must be a function handle."),
            name);
        return false;
    }
    callback = value;
    callback.ensureSingleOwner();
    return true;
}
//=============================================================================
void
enqueueAudioCallback(const ArrayOf& callback, HandleGenericObject* source)
{
    if (callback.isEmpty()) {
        return;
    }
    if (callback.isScalarStringArray() || callback.isRowVectorCharacterArray()) {
        EventQueue::getInstance()->add(EventCallback(callback));
    } else if (callback.isFunctionHandle()) {
        size_t nbElements = 3;
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        elements[0] = callback;
        elements[1] = ArrayOf::handle(source);
        elements[2] = ArrayOf::empty(0, 0);
        EventQueue::getInstance()->add(
            EventCallback(ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements)));
    }
}
//=============================================================================
std::wstring
audioCallbackToWideString(const ArrayOf& callback)
{
    if (callback.isEmpty()) {
        return L"''";
    }
    if (callback.isFunctionHandle()) {
        function_handle fh = callback.asFunctionHandle();
        auto* anonymousFunction = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        return utf8_to_wstring(anonymousFunction->getDefinition());
    }
    if (callback.isCell()) {
        return L"{" + callback.dimensions().toWideString() + L" cell}";
    }
    if (callback.isRowVectorCharacterArray() || callback.isScalarStringArray()) {
        return L"'" + callback.asWideString() + L"'";
    }
    return L"";
}
//=============================================================================
std::wstring
eventLoopRequiredMessage(const std::wstring& name)
{
    if (name == L"StartFcn") {
        return resolveMessageTemplate(
            L"Nelson:audio:cannotSetStartfcnWithout", L"Cannot set StartFcn without event loop.");
    }
    if (name == L"StopFcn") {
        return resolveMessageTemplate(
            L"Nelson:audio:cannotSetStopfcnWithout", L"Cannot set StopFcn without event loop.");
    }
    return resolveMessageTemplate(
        L"Nelson:audio:cannotSetTimerfcnWithout", L"Cannot set TimerFcn without event loop.");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
