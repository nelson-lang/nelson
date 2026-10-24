//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include "nlsGraphics_exports.h"
//=============================================================================
class RenderInterface;
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIComponent;
//=============================================================================
// Qt-free view of the native widget backing an App Designer component. The model only
// talks to this interface; graphics_qt implements it and registers a factory below at load
// time. Headless hosts (nelson-webview) never create bindings.
class UIComponentBinding
{
public:
    virtual ~UIComponentBinding() = default;
    //=============================================================================
    // False when no parent widget exists yet.
    virtual bool
    create()
        = 0;
    virtual bool
    exists() const
        = 0;
    //=============================================================================
    virtual void
    applyCommonProperties(bool isNewWidget)
        = 0;
    virtual void
    applyState(bool isNewWidget)
        = 0;
    //=============================================================================
    virtual void
    show()
        = 0;
    virtual void
    hide()
        = 0;
    virtual void
    setFocus()
        = 0;
    // The binding itself stays reusable.
    virtual void
    destroy()
        = 0;
    //=============================================================================
    // QWidget*, for parent/child wiring inside the backend.
    virtual void*
    nativeWidget()
        = 0;
    // Widget hosting the children of a container. Differs from the widget for uitab pages.
    virtual void*
    nativeClientWidget()
    {
        return nativeWidget();
    }
    //=============================================================================
    // True when the geometry changed since the last call. Drives SizeChangedFcn and grid
    // re-layout.
    virtual bool
    consumeGeometryChange()
        = 0;
    //=============================================================================
    // For the non-GL export paths.
    virtual void
    renderSnapshot(RenderInterface& gc)
        = 0;
    //=============================================================================
};
//=============================================================================
using UIComponentBindingFactory = UIComponentBinding* (*)(GOUIComponent * component);
//=============================================================================
NLSGRAPHICS_IMPEXP void
setUIComponentBindingFactory(UIComponentBindingFactory factory);
NLSGRAPHICS_IMPEXP UIComponentBindingFactory
getUIComponentBindingFactory();
//=============================================================================
// Classic uicontrol path: same interface, separate factory. GOUIControl is not a
// GOUIComponent.
class GOUIControl;
using UIControlBindingFactory = UIComponentBinding* (*)(GOUIControl * control);
NLSGRAPHICS_IMPEXP void
setUIControlBindingFactory(UIControlBindingFactory factory);
NLSGRAPHICS_IMPEXP UIControlBindingFactory
getUIControlBindingFactory();
//=============================================================================
// Test hook (__uicontrol_key_event__): delivers a key ("space", "up", "down", "home", "end",
// "tab" or "shift+tab") to the widget of a uicontrol and reports the uicontrol holding the
// focus afterwards. Registered by the widget backend; absent on headless hosts.
using UIControlKeyInjector
    = bool (*)(GOUIControl* control, const std::wstring& key, GOUIControl** focused);
NLSGRAPHICS_IMPEXP void
setUIControlKeyInjector(UIControlKeyInjector injector);
NLSGRAPHICS_IMPEXP UIControlKeyInjector
getUIControlKeyInjector();
//=============================================================================
} // namespace Nelson
//=============================================================================
