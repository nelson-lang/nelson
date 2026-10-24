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
#include "GOUIComponent.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIPanel;
//=============================================================================
// uiradiobutton: radio button control
// uitogglebutton: toggle button control
// Both live inside a uibuttongroup which manages the exclusive selection.
class NLSGRAPHICS_IMPEXP GOUIRadioButton : public GOUIComponent
{
    bool toggleStyle;

public:
    //=============================================================================
    explicit GOUIRadioButton(bool isToggleButton = false);
    ~GOUIRadioButton() override = default;
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
    bool
    isToggleButton() const
    {
        return toggleStyle;
    }
    //=============================================================================
    // called by the parent group to clear/set the selection state
    void
    setSelected(bool selected);
    //=============================================================================
    // a user click (Qt widget or web control): selects this button and lets its
    // group clear the previous selection and fire SelectionChanged. Clicking the
    // selected button keeps it selected.
    void
    onUserClick();
    //=============================================================================
    // the uibuttongroup holding this button, nullptr otherwise
    GOUIPanel*
    parentButtonGroup();
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
