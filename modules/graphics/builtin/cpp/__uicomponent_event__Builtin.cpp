//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__uicomponent_event__Builtin.hpp"
#include "GOHelpers.hpp"
#include "GOUIComponent.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __uicomponent_event__(h, action [, a, b]): test hook that fires a uifigure
// component's interaction callback headless, building the same event data the
// Qt widget binding would, so the callbacks can be runtime-verified without a
// display. action is 'doubleclick' (listbox/table), 'click' (table),
// 'expand'/'collapse' (tree, a = node handle) or 'opening' (dropdown); for a
// table, a = row and b = column (1-based). Returns true when a callback fired.
ArrayOfVector
Nelson::GraphicsGateway::__uicomponent_event__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 4);
    nargoutcheck(nLhs, 0, 1);
    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object.");
    }
    const nelson_handle handle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(handle)) {
        Error(L"Nelson:graphics:invalidGraphicsObject", L"Valid graphics object expected.");
    }
    GraphicsObject* object = findGraphicsObject(handle, false);
    GOUIComponent* component = dynamic_cast<GOUIComponent*>(object);
    if (component == nullptr) {
        Error(L"Nelson:graphics:expectedUIComponent", L"UI component expected.");
    }
    std::wstring action = argIn[1].asWideString();
    std::wstring type = object->getType();

    std::wstring callbackName;
    std::wstring eventClass;
    std::wstring eventName;
    wstringVector fieldNames;
    ArrayOfVector fieldValues;

    if (type == GO_PROPERTY_VALUE_UILISTBOX_STR && action == L"doubleclick") {
        callbackName = GO_DOUBLE_CLICKED_FCN_PROPERTY_NAME_STR;
        eventClass = L"DoubleClickedData";
        eventName = L"DoubleClicked";
    } else if (type == GO_PROPERTY_VALUE_UIDROPDOWN_STR && action == L"opening") {
        callbackName = GO_DROP_DOWN_OPENING_FCN_PROPERTY_NAME_STR;
        eventClass = L"DropDownOpeningData";
        eventName = L"DropDownOpening";
    } else if ((type == GO_PROPERTY_VALUE_UITREE_STR
                   || type == GO_PROPERTY_VALUE_UICHECKBOXTREE_STR)
        && (action == L"expand" || action == L"collapse")) {
        if (argIn.size() < 3 || !argIn[2].isScalar()
            || (!argIn[2].isGraphicsObject() && !argIn[2].isNumeric())) {
            Error(L"Nelson:graphics:expectedNode", L"Node handle expected for a tree event.");
        }
        const nelson_handle nodeHandle = argIn[2].isGraphicsObject()
            ? static_cast<const nelson_handle*>(argIn[2].dataPointer())[0]
            : static_cast<nelson_handle>(argIn[2].asDoubleScalar());
        callbackName = action == L"expand" ? GO_NODE_EXPANDED_FCN_PROPERTY_NAME_STR
                                           : GO_NODE_COLLAPSED_FCN_PROPERTY_NAME_STR;
        eventClass = action == L"expand" ? L"NodeExpandedData" : L"NodeCollapsedData";
        eventName = action == L"expand" ? L"NodeExpanded" : L"NodeCollapsed";
        fieldNames = { L"Node" };
        fieldValues << graphicsObjectHandle(static_cast<int64>(nodeHandle), false);
    } else if (type == GO_PROPERTY_VALUE_UITABLE_STR
        && (action == L"click" || action == L"doubleclick")) {
        if (argIn.size() < 4 || !argIn[2].isScalar() || !argIn[3].isScalar()) {
            Error(L"Nelson:graphics:expectedCell", L"Row and column expected for a table event.");
        }
        double row = argIn[2].asDoubleScalar();
        double column = argIn[3].asDoubleScalar();
        callbackName = action == L"click" ? GO_CLICKED_FCN_PROPERTY_NAME_STR
                                          : GO_DOUBLE_CLICKED_FCN_PROPERTY_NAME_STR;
        eventClass = action == L"click" ? L"ClickedData" : L"DoubleClickedData";
        eventName = action == L"click" ? L"Clicked" : L"DoubleClicked";
        fieldNames = { L"Row", L"Column" };
        fieldValues << ArrayOf::doubleScalar(row);
        fieldValues << ArrayOf::doubleScalar(column);
    } else {
        Error(L"Nelson:graphics:unsupportedUIComponentEvent",
            L"Unsupported component / action combination.");
    }

    bool fired = false;
    GOGenericProperty* cb = component->findProperty(callbackName, false);
    if (cb != nullptr && !cb->get().isEmpty()) {
        component->emitCallbackEvent(callbackName, eventClass, eventName, fieldNames, fieldValues,
            true);
        fired = true;
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(fired);
    return retval;
}
//=============================================================================
