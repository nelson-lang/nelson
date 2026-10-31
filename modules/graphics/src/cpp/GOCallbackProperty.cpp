//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOCallbackProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "AnonymousMacroFunctionDef.hpp"
#include "characters_encoding.hpp"
#include "GOHelpers.hpp"
#include "CallbackQueue.hpp"
#include "NelsonConfiguration.hpp"
#include "Evaluator.hpp"
#include <memory>
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
graphicsObjectCallbackHandle(GraphicsObject* go)
{
    int64 handle = findGraphicsObjectHandle(go);
    if (handle == -1) {
        Error(L"Nelson:graphics:objectNotRegistered", L"Graphics object is not registered.");
    }
    return graphicsObjectHandle(handle);
}
//=============================================================================
GraphicCallback
GOCallbackProperty::buildGraphicCallback(GraphicsObject* go, const std::wstring& className,
    const std::wstring& EventName, const std::wstring& character, const std::wstring& key,
    const wstringVector& modifiers)
{
    ArrayOf callbackAsArrayOf;
    ArrayOf source = graphicsObjectCallbackHandle(go);
    int64 sourceHandle = source.asGraphicsObjectScalar();
    if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        callbackAsArrayOf = _data;
    } else if (_data.isFunctionHandle()) {
        size_t nbElements = 3;
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        elements[0] = _data;
        elements[1] = source;
        wstringVector fieldnames = { L"Character", L"Modifier", L"Key", L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(fieldnames.size());
        fieldvalues << ArrayOf::characterArray(character);
        fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(modifiers);
        fieldvalues << ArrayOf::characterArray(key);
        fieldvalues << source;
        fieldvalues << ArrayOf::characterArray(EventName);
        elements[2] = ArrayOf::classArray(className, fieldnames, fieldvalues);
        callbackAsArrayOf = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
    } else if (_data.isCell()) {
        wstringVector fieldnames = { L"Character", L"Modifier", L"Key", L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(fieldnames.size());
        fieldvalues << ArrayOf::characterArray(character);
        fieldvalues << ArrayOf::cellArrayFromArrayOfCharacterColumnVectors(modifiers);
        fieldvalues << ArrayOf::characterArray(key);
        fieldvalues << source;
        fieldvalues << ArrayOf::characterArray(EventName);
        ArrayOf* cellElements = (ArrayOf*)_data.dataPointer();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.elementCount() + 2);
        elements[0] = cellElements[0];
        elements[1] = source;
        elements[2] = ArrayOf::classArray(className, fieldnames, fieldvalues);
        for (size_t k = 1; k < _data.elementCount(); ++k) {
            elements[2 + k] = cellElements[k];
        }
        callbackAsArrayOf
            = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, _data.elementCount() + 2), elements);
    } else {
        Error("Nelson:graphics:caseNotManaged", "Case not yet managed.");
    }
    GraphicCallback graphicCallback(
        go->stringCheck(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR),
        go->stringCheck(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR)
            ? BUSY_ACTION::QUEUE
            : BUSY_ACTION::CANCEL,
        callbackAsArrayOf, sourceHandle);
    return graphicCallback;
}
//=============================================================================
GraphicCallback
GOCallbackProperty::buildGraphicCallback(
    GraphicsObject* go, const std::wstring& className, const std::wstring& actionName)
{
    ArrayOf callbackAsArrayOf;
    ArrayOf source = graphicsObjectCallbackHandle(go);
    int64 sourceHandle = source.asGraphicsObjectScalar();
    if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        callbackAsArrayOf = _data;
    } else if (_data.isFunctionHandle()) {
        size_t nbElements = 3;
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        elements[0] = _data;
        elements[1] = source;
        wstringVector fieldnames = { L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(2);
        fieldvalues << source;
        fieldvalues << ArrayOf::characterArray(actionName);
        elements[2] = ArrayOf::classArray(className, fieldnames, fieldvalues);
        callbackAsArrayOf = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
    } else if (_data.isCell()) {
        ArrayOf* cellElements = (ArrayOf*)_data.dataPointer();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.elementCount() + 2);
        elements[0] = cellElements[0];
        elements[1] = source;
        wstringVector fieldnames = { L"Source", L"EventName" };
        ArrayOfVector fieldvalues;
        fieldvalues.reserve(2);
        fieldvalues << source;
        fieldvalues << ArrayOf::characterArray(actionName);
        elements[2] = ArrayOf::classArray(className, fieldnames, fieldvalues);
        for (size_t k = 1; k < _data.elementCount(); ++k) {
            elements[2 + k] = cellElements[k];
        }
        callbackAsArrayOf
            = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, _data.elementCount() + 2), elements);
    }

    GraphicCallback graphicCallback(
        go->stringCheck(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR),
        go->stringCheck(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR)
            ? BUSY_ACTION::QUEUE
            : BUSY_ACTION::CANCEL,
        callbackAsArrayOf, sourceHandle);
    return graphicCallback;
}
//=============================================================================
GraphicCallback
GOCallbackProperty::buildGraphicCallback(GraphicsObject* go, const std::wstring& className,
    const std::wstring& eventName, const wstringVector& fieldNames,
    const ArrayOfVector& fieldValues)
{
    ArrayOf callbackAsArrayOf;
    ArrayOf source = graphicsObjectCallbackHandle(go);
    int64 sourceHandle = source.asGraphicsObjectScalar();
    if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        callbackAsArrayOf = _data;
    } else if (_data.isFunctionHandle() || _data.isCell()) {
        wstringVector eventFieldNames(fieldNames);
        ArrayOfVector eventFieldValues(fieldValues);
        eventFieldNames.push_back(L"Source");
        eventFieldValues << source;
        eventFieldNames.push_back(L"EventName");
        eventFieldValues << ArrayOf::characterArray(eventName);
        ArrayOf eventData = ArrayOf::classArray(className, eventFieldNames, eventFieldValues);

        if (_data.isFunctionHandle()) {
            size_t nbElements = 3;
            ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
            elements[0] = _data;
            elements[1] = source;
            elements[2] = eventData;
            callbackAsArrayOf = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
        } else {
            ArrayOf* cellElements = (ArrayOf*)_data.dataPointer();
            ArrayOf* elements
                = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.elementCount() + 2);
            elements[0] = cellElements[0];
            elements[1] = source;
            elements[2] = eventData;
            for (size_t k = 1; k < _data.elementCount(); ++k) {
                elements[2 + k] = cellElements[k];
            }
            callbackAsArrayOf
                = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, _data.elementCount() + 2), elements);
        }
    } else {
        Error("Nelson:graphics:caseNotManaged", "Case not yet managed.");
    }
    GraphicCallback graphicCallback(
        go->stringCheck(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR),
        go->stringCheck(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR)
            ? BUSY_ACTION::QUEUE
            : BUSY_ACTION::CANCEL,
        callbackAsArrayOf, sourceHandle);
    return graphicCallback;
}
//=============================================================================
GraphicCallback
GOCallbackProperty::buildGraphicCallback(GraphicsObject* go)
{
    ArrayOf callbackAsArrayOf;
    ArrayOf source = graphicsObjectCallbackHandle(go);
    int64 sourceHandle = source.asGraphicsObjectScalar();
    if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        callbackAsArrayOf = _data;
    } else if (_data.isFunctionHandle()) {
        size_t nbElements = 3;
        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        elements[0] = _data;
        elements[1] = source;
        elements[2] = ArrayOf::empty(0, 0);
        callbackAsArrayOf = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, nbElements), elements);
    } else if (_data.isCell()) {
        ArrayOf* cellElements = (ArrayOf*)_data.dataPointer();
        ArrayOf* elements
            = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, _data.elementCount() + 2);
        elements[0] = cellElements[0];
        elements[1] = source;
        elements[2] = ArrayOf::empty(0, 0);

        for (size_t k = 1; k < _data.elementCount(); ++k) {
            elements[2 + k] = cellElements[k];
        }
        callbackAsArrayOf
            = ArrayOf(NLS_CELL_ARRAY, Dimensions(1, _data.elementCount() + 2), elements);
    }
    GraphicCallback graphicCallback(
        go->stringCheck(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR),
        go->stringCheck(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR)
            ? BUSY_ACTION::QUEUE
            : BUSY_ACTION::CANCEL,
        callbackAsArrayOf, sourceHandle);
    return graphicCallback;
}
//=============================================================================
void
GOCallbackProperty::set(ArrayOf arg)
{
    if (arg.isEmpty()) {
        GOGenericProperty::set(ArrayOf::characterArray(""));
        GOArrayOfProperty::set(ArrayOf::characterArray(""));
        return;
    }
    if (arg.isScalarStringArray() || arg.isRowVectorCharacterArray()) {
        std::wstring asString = arg.asWideString();
        GOGenericProperty::set(ArrayOf::characterArray(asString));
        GOArrayOfProperty::set(ArrayOf::characterArray(asString));
        return;
    }

    GOGenericProperty::set(arg);
    bool isSupportedType = false;
    if (arg.isCell()) {
        isSupportedType = arg.isScalar() || arg.isRowVector();
        if (isSupportedType) {
            ArrayOf* elements = (ArrayOf*)arg.dataPointer();
            isSupportedType = elements[0].isFunctionHandle();
        }
    } else {
        isSupportedType = arg.isScalar() && arg.isFunctionHandle();
    }

    if (!isSupportedType) {
        std::wstring msg
            = resolveMessageTemplate(L"Nelson:graphics:callbackCharacterVectorFunction",
                L"Callback value must be a character vector, a function handle, or a cell array "
                L"containing character vector or function handle.");
        Error(L"Nelson:graphics:invalidCallbackValue", msg);
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
        function_handle fh = _data.asFunctionHandle();
        AnonymousMacroFunctionDef* anonymousFunction
            = reinterpret_cast<AnonymousMacroFunctionDef*>(fh.anonymousHandle);
        return utf8_to_wstring(anonymousFunction->getDefinition());
    } else if (_data.isCell()) {
        Dimensions dims = _data.dimensions();
        return L"{" + dims.toWideString() + L" cell}";
    } else if (_data.isRowVectorCharacterArray() || _data.isScalarStringArray()) {
        std::unique_ptr<wchar_t[]> buffer(_data.toAllocatedWideCString());
        return std::wstring(L"'") + buffer.get() + std::wstring(L"'");
    }
    return L"";
}
//=============================================================================
void
GOCallbackProperty::pushEvent(GraphicsObject* go)
{
    GraphicCallback graphicCallback = buildGraphicCallback(go);
    CallbackQueue::getInstance()->add(graphicCallback);
}
//=============================================================================
void
GOCallbackProperty::executeNow(GraphicsObject* go)
{
    GraphicCallback graphicCallback = buildGraphicCallback(go);
    graphicCallback.execute((Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator());
}
//=============================================================================
void
GOCallbackProperty::pushEvent(
    GraphicsObject* go, const std::wstring& className, const std::wstring& actionName)
{
    GraphicCallback graphicCallback = buildGraphicCallback(go, className, actionName);
    CallbackQueue::getInstance()->add(graphicCallback);
}
//=============================================================================
void
GOCallbackProperty::pushEvent(GraphicsObject* go, const std::wstring& className,
    const std::wstring& eventName, const wstringVector& fieldNames,
    const ArrayOfVector& fieldValues)
{
    GraphicCallback graphicCallback
        = buildGraphicCallback(go, className, eventName, fieldNames, fieldValues);
    CallbackQueue::getInstance()->add(graphicCallback);
}
//=============================================================================
void
GOCallbackProperty::pushKeyEvent(GraphicsObject* go, const std::wstring& className,
    const std::wstring& EventName, const std::wstring& character, const std::wstring& key,
    const wstringVector& modifiers)
{
    GraphicCallback graphicCallback
        = buildGraphicCallback(go, className, EventName, character, key, modifiers);
    CallbackQueue::getInstance()->add(graphicCallback);
}
//=============================================================================
void
GOCallbackProperty::executeNow(
    GraphicsObject* go, const std::wstring& className, const std::wstring& actionName)
{
    GraphicCallback graphicCallback = buildGraphicCallback(go, className, actionName);
    graphicCallback.execute((Evaluator*)NelsonConfiguration::getInstance()->getMainEvaluator());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
