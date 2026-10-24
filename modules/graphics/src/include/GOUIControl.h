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
class RenderWeb;
//=============================================================================
// Classic uicontrol model (pushbutton, slider, listbox, ...). The native
// widget hosted in the figure window is owned by a UIComponentBinding created
// through the registered uicontrol factory (graphics_qt). The model itself is
// widget-toolkit free; headless hosts (nelson-webview) never create a binding and
// use describeForWeb / applyWebAction only.
class NLSGRAPHICS_IMPEXP GOUIControl : public GraphicsObject
{
    UIComponentBinding* binding = nullptr;
    GOFigureHost* parentGoWindow = nullptr;

public:
    //=============================================================================
    GOUIControl();
    //=============================================================================
    ~GOUIControl() override;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_UI_CONTROL_STR;
    }
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    void
    paintMe(RenderInterface& gc) override;

    // Emits a description of the control instead of a raster, so the frontend builds an
    // interactive HTML widget.
    void
    describeForWeb(RenderWeb& renderer, const std::vector<double>& position);

    // Stores the new Value/String, then runs the Callback like the Qt widget signal does.
    void
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
    //=============================================================================
    void
    hide();
    //=============================================================================
    void
    setFocus();
    //=============================================================================
    // Model part of a user click. The widget backend syncs Value/String from the widget
    // first, then calls this.
    void
    clicked();
    //=============================================================================
    // Radio/toggle button held by a uibuttongroup: a click selects it (the group clears the
    // previous one and fires SelectionChangedFcn) and never clears the selected one. Returns
    // true when the group manages the button, so its own Callback must not run.
    bool
    onGroupButtonClicked();
    //=============================================================================
    // A Value set to Max from code selects the button in its group (no event).
    void
    syncButtonGroupSelection();
    //=============================================================================
    // True when Enable is 'inactive': the widget looks normal but ignores user input (no
    // editing, toggling, value change or Action callback). ButtonDownFcn still fires.
    bool
    isEnableInactive();
    //=============================================================================
    // Converts Position when Units changes. Called by the binding in the middle of the
    // property-application sequence.
    void
    onUnitsChanged(bool newWidget);
    //=============================================================================
    void
    tokenize(
        const std::wstring& str, std::vector<std::wstring>& tokens, const std::wstring& delimiters);
    //=============================================================================
private:
    //=============================================================================
    virtual void
    constructProperties();
    //=============================================================================
    void
    setupDefaults();
    //=============================================================================
    void
    deleteWidget();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
