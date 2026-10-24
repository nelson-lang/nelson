//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "UIComponentBindings.hpp"
#include "GOUIButton.h"
#include "GOUICheckBox.h"
#include "GOUIDatePicker.h"
#include "GOUIDropDown.h"
#include "GOUIEditField.h"
#include "GOUIGauge.h"
#include "GOUIGridLayout.h"
#include "GOUIKnob.h"
#include "GOUILabel.h"
#include "GOUILamp.h"
#include "GOUIListBox.h"
#include "GOUIPanel.h"
#include "GOUIRadioButton.h"
#include "GOUISlider.h"
#include "GOUISpinner.h"
#include "GOUISwitch.h"
#include "GOUITab.h"
#include "GOUITabGroup.h"
#include "GOUITable.h"
#include "GOUITextArea.h"
#include "GOUITree.h"
//=============================================================================
namespace Nelson {
//=============================================================================
UIComponentBinding*
createUIComponentBinding(GOUIComponent* component)
{
    if (dynamic_cast<GOUIButton*>(component)) {
        return createUIButtonBinding(component);
    }
    if (dynamic_cast<GOUICheckBox*>(component)) {
        return createUICheckBoxBinding(component);
    }
    if (dynamic_cast<GOUIDatePicker*>(component)) {
        return createUIDatePickerBinding(component);
    }
    if (dynamic_cast<GOUIDropDown*>(component)) {
        return createUIDropDownBinding(component);
    }
    if (dynamic_cast<GOUIEditField*>(component)) {
        return createUIEditFieldBinding(component);
    }
    if (dynamic_cast<GOUIGauge*>(component)) {
        return createUIGaugeBinding(component);
    }
    if (dynamic_cast<GOUIGridLayout*>(component)) {
        return createUIGridLayoutBinding(component);
    }
    if (dynamic_cast<GOUIKnob*>(component)) {
        return createUIKnobBinding(component);
    }
    if (dynamic_cast<GOUILabel*>(component)) {
        return createUILabelBinding(component);
    }
    if (dynamic_cast<GOUILamp*>(component)) {
        return createUILampBinding(component);
    }
    if (dynamic_cast<GOUIListBox*>(component)) {
        return createUIListBoxBinding(component);
    }
    if (dynamic_cast<GOUIPanel*>(component)) {
        return createUIPanelBinding(component);
    }
    if (dynamic_cast<GOUIRadioButton*>(component)) {
        return createUIRadioButtonBinding(component);
    }
    if (dynamic_cast<GOUISlider*>(component)) {
        return createUISliderBinding(component);
    }
    if (dynamic_cast<GOUISpinner*>(component)) {
        return createUISpinnerBinding(component);
    }
    if (dynamic_cast<GOUISwitch*>(component)) {
        return createUISwitchBinding(component);
    }
    if (dynamic_cast<GOUITab*>(component)) {
        return createUITabBinding(component);
    }
    if (dynamic_cast<GOUITabGroup*>(component)) {
        return createUITabGroupBinding(component);
    }
    if (dynamic_cast<GOUITable*>(component)) {
        return createUITableBinding(component);
    }
    if (dynamic_cast<GOUITextArea*>(component)) {
        return createUITextAreaBinding(component);
    }
    if (dynamic_cast<GOUITree*>(component)) {
        return createUITreeBinding(component);
    }
    // uitreenode and other widget-less models
    return nullptr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
