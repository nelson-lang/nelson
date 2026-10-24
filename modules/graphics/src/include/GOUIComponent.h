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
#include <vector>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "GOFigureHost.hpp"
#include "GOUIWidgetBinding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class RenderWeb;
struct WebUIControl;
//=============================================================================
// Base class for app-style UI components (uibutton, uilabel,
// uidropdown, ...). Each component exposes a compatible property set; the
// native widget hosted in the figure window is owned by a UIComponentBinding
// created through the registered factory (graphics_qt). The model itself is
// widget-toolkit free.
class NLSGRAPHICS_IMPEXP GOUIComponent : public GraphicsObject
{
protected:
    UIComponentBinding* binding = nullptr;
    GOFigureHost* parentGoWindow = nullptr;
    bool programmaticChange = false;

public:
    //=============================================================================
    GOUIComponent();
    ~GOUIComponent() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    // nelson-webview path: describe this component as an interactive HTML control
    // (no Qt rasterization), and apply a browser interaction back onto the
    // object, running the matching callback through the CallbackQueue.
    void
    describeForWeb(RenderWeb& renderer, const std::vector<double>& position);
    bool
    applyWebAction(double value, const std::string& text);
    //=============================================================================
    void
    buildWidget(GOFigureHost* f);
    //=============================================================================
    UIComponentBinding*
    getBinding()
    {
        return binding;
    }
    //=============================================================================
    GOFigureHost*
    getParentFigureHost()
    {
        return parentGoWindow;
    }
    //=============================================================================
    void
    show();
    void
    hide();
    void
    setFocus();
    //=============================================================================
    bool
    isProgrammaticChange() const
    {
        return programmaticChange;
    }
    //=============================================================================
    // true when the component geometry is managed by its parent
    // (uitab pages, uigridlayout filling its parent, grid children)
    virtual bool
    managesOwnGeometry()
    {
        return false;
    }
    //=============================================================================
    virtual void
    onGeometryChanged()
    {
    }
    //=============================================================================
    // queue the execution of a callback property with a compatible event
    // object; extra event fields (Value, PreviousValue, ...) come first,
    // Source and EventName are appended automatically. Public: the widget
    // binding forwards user interactions through this.
    void
    emitCallbackEvent(const std::wstring& callbackPropertyName,
        const std::wstring& eventDataClassName, const std::wstring& eventName,
        const wstringVector& fieldNames = {}, const ArrayOfVector& fieldValues = {},
        bool evenIfProgrammatic = false);
    //=============================================================================
    // sync InnerPosition/OuterPosition after a Position write (used by the
    // widget binding once it applied the geometry).
    virtual void
    syncPositionProperties();
    //=============================================================================
    void
    onUnitsChanged(bool newWidget);
    //=============================================================================
    // RAII guard: widget signals triggered while active do not fire user
    // callbacks (programmatic property changes). Public: the binding wraps
    // its own programmatic widget updates with it.
    class ProgrammaticGuard
    {
        GOUIComponent* m_component;

    public:
        ProgrammaticGuard(GOUIComponent* component) : m_component(component)
        {
            m_component->programmaticChange = true;
        }
        ~ProgrammaticGuard() { m_component->programmaticChange = false; }
    };
    //=============================================================================
protected:
    //=============================================================================
    void
    deleteWidget();
    //=============================================================================
    void
    registerUIComponentCommonProperties(
        bool withFonts = true, bool withBackgroundColor = true, bool withVisibleChildren = false);
    void
    setupUIComponentCommonDefaults(double x, double y, double w, double h, bool withFonts = true,
        bool withBackgroundColor = true);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
