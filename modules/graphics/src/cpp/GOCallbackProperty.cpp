//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QApplication>
#include "GOCallbackProperty.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "GOHelpers.hpp"
#include "ProcessEvents.hpp"
#include "CallbackQueue.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOCallbackProperty::set(ArrayOf arg)
{
    if (arg.isEmpty()) {
        GOGenericProperty::set(ArrayOf::characterArrayConstructor(""));
        GOArrayOfProperty::set(ArrayOf::characterArrayConstructor(""));
        return;
    }
    if (arg.isScalarStringArray() || arg.isRowVectorCharacterArray()) {
        std::wstring asString = arg.getContentAsWideString();
        GOGenericProperty::set(ArrayOf::characterArrayConstructor(asString));
        GOArrayOfProperty::set(ArrayOf::characterArrayConstructor(asString));
        return;
    }

    GOGenericProperty::set(arg);
    bool isSupportedType = false;
    if (arg.isCell()) {
        isSupportedType = arg.isScalar() || arg.isRowVector();
        if (isSupportedType) {
            ArrayOf* elements = (ArrayOf*)arg.getDataPointer();
            isSupportedType = elements[0].isFunctionHandle();
        }
    } else {
        isSupportedType = arg.isScalar() && arg.isFunctionHandle();
    }

    if (!isSupportedType) {
        std::wstring msg = _W("Callback value must be a character vector, a function handle, or a "
                              "cell array containing character vector or function handle.");
        Error(msg);
    }
    GOArrayOfProperty::set(arg);
}
//=============================================================================
ArrayOf
GOCallbackProperty::get()
{
    return _data;
}
//=============================================================================
std::wstring
GOCallbackProperty::toWideString()
{
    if (_data.isEmpty()) {
        return L"''";
    } else if (_data.isFunctionHandle()) {
        function_handle fh = _data.getContentAsFunctionHandle();
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        return utf8_to_wstring(anonymousFunction->getDefinition());
    } else if (_data.isCell()) {
        Dimensions dims = _data.getDimensions();
        return L"{" + dims.toWideString() + L" cell}";
    } else if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        std::wstring callbackString
            = std::wstring(L"'") + _data.getContentAsWideCharactersPointer() + std::wstring(L"'");
        return callbackString;
    }
    return L"";
}
//=============================================================================
bool
GOCallbackProperty::pushEvent(
    GraphicsObject* go, const std::wstring& className, const std::wstring& actionName)
{
    ArrayOf callbackAsArrayOf;
    if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        callbackAsArrayOf = _data;
    } else if (_data.isFunctionHandle()) {
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, 4);
        elements[0] = _data;
        elements[1] = ArrayOf::graphicsObjectConstructor(assignGraphicsObject(go));
        wstringVector fieldnames = { L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(2);
        fieldvalues << ArrayOf::graphicsObjectConstructor(assignGraphicsObject(go));
        fieldvalues << ArrayOf::characterArrayConstructor(actionName);
        elements[3] = ArrayOf::classConstructor(className, fieldnames, fieldvalues);
        callbackAsArrayOf = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, 4), elements);
    } else if (_data.isCell()) {
        ArrayOf* cellElements = (ArrayOf*)_data.getDataPointer();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.getElementCount() + 2);
        elements[0] = cellElements[0];
        elements[1] = ArrayOf::graphicsObjectConstructor(assignGraphicsObject(go));
        wstringVector fieldnames = { L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(2);
        fieldvalues << ArrayOf::graphicsObjectConstructor(assignGraphicsObject(go));
        fieldvalues << ArrayOf::characterArrayConstructor(actionName);
        elements[3] = ArrayOf::classConstructor(className, fieldnames, fieldvalues);
        for (size_t k = 1; k < _data.getElementCount(); ++k) {
            elements[3 + k] = cellElements[k];
        }
        callbackAsArrayOf
            = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, _data.getElementCount() + 2), elements);
    } else {
        return false;
    }

    GraphicCallback graphicCallback(callbackAsArrayOf);
    CallbackQueue::getInstance()->add(graphicCallback);
    return true;
}
//=============================================================================
}
//=============================================================================
