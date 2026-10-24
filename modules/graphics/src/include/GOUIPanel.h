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
#include "GOUIContainer.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// uipanel: panel container
// uibuttongroup: button group container (adds exclusive selection
// management of radio/toggle button children).
class NLSGRAPHICS_IMPEXP GOUIPanel : public GOUIContainer
{
    bool buttonGroupStyle;

public:
    //=============================================================================
    explicit GOUIPanel(bool isButtonGroup = false);
    ~GOUIPanel() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    updateState() override;
    //=============================================================================
    void
    syncPositionProperties() override;
    //=============================================================================
    void
    paintChrome(RenderInterface& gc) override;
    //=============================================================================
    std::vector<double>
    contentRect(RenderInterface& gc) override;
    //=============================================================================
    bool
    isButtonGroup() const
    {
        return buttonGroupStyle;
    }
    //=============================================================================
    // called by radio/toggle button children when they become selected;
    // fireEvent is false for programmatic Value changes
    void
    notifyChildSelected(int64 childHandle, bool fireEvent = true);
    //=============================================================================
    // Registers a radio/toggle button (ui component or uicontrol) created in the
    // group. It becomes the selected one when it is created selected, or when it
    // is the first button and its Value was not given explicitly.
    void
    addGroupButton(int64 buttonHandle, bool selected, bool valueGiven);
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    void
    synchronizePanelPositions();
    // Buttons: the radio/toggle children (ui components or uicontrols).
    void
    refreshGroupButtons();
    // SelectedObject set from code: select that button, clear the others.
    void
    applySelectedObject();
    void
    deriveInnerPosition();
    // Border thickness actually painted (0 when BorderType is 'none').
    double
    panelBorderWidth();
    // Height the Title band takes at the top of the panel, 0 without a title.
    double
    titleBandHeight(RenderInterface& gc);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
