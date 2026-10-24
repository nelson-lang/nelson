//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__uicontrol_key_event__Builtin.hpp"
#include "GOHelpers.hpp"
#include "GOUIControl.h"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// [delivered, focused] = __uicontrol_key_event__(h, key): delivers a key ('space',
// 'up', 'down', 'home', 'end', 'tab' or 'shift+tab') to the widget of uicontrol h
// as the keyboard does. delivered is false when no widget backend is available
// (headless host); focused is the uicontrol holding the focus afterwards.
ArrayOfVector
Nelson::GraphicsGateway::__uicontrol_key_event__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isGraphicsObject() || !argIn[0].isScalar()) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObject",
            L"Expected graphics object.");
    }
    auto* gobject = static_cast<const nelson_handle*>(argIn[0].dataPointer());
    GraphicsObject* object
        = isDeletedGraphicsObject(gobject[0]) || gobject[0] < HANDLE_OFFSET_OBJECT
        ? nullptr
        : findGraphicsObject(gobject[0], false);
    auto* control = dynamic_cast<GOUIControl*>(object);
    if (control == nullptr) {
        Error(L"Nelson:uicontrol:expectedUicontrol", L"uicontrol graphic object expected.");
    }
    std::wstring key = argIn[1].asWideString();
    UIControlKeyInjector injector = getUIControlKeyInjector();
    GOUIControl* focused = nullptr;
    bool delivered = injector != nullptr && injector(control, key, &focused);
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(delivered);
    if (nLhs > 1) {
        retval << (focused ? graphicsObjectHandle(findGraphicsObjectHandle(focused))
                           : ArrayOf::empty(0, 0));
    }
    return retval;
}
//=============================================================================
