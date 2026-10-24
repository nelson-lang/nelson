//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIRadioButton.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOUIPanel.h"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIRadioButton::GOUIRadioButton(bool isToggleButton) : toggleStyle(isToggleButton)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIRadioButton::getType()
{
    return toggleStyle ? GO_PROPERTY_VALUE_UITOGGLEBUTTON_STR : GO_PROPERTY_VALUE_UIRADIOBUTTON_STR;
}
//=============================================================================
std::string
GOUIRadioButton::getClassName()
{
    return toggleStyle ? GO_CLASS_NAME_UI_TOGGLEBUTTON_STR : GO_CLASS_NAME_UI_RADIOBUTTON_STR;
}
//=============================================================================
void
GOUIRadioButton::updateState()
{
    if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)
        && static_cast<GOScalarLogicalProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR))
            ->data()) {
        GOUIPanel* buttonGroup = parentButtonGroup();
        if (buttonGroup) {
            buttonGroup->notifyChildSelected(findGraphicsObjectHandle(this), false);
        }
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUIRadioButton::onUserClick()
{
    GOScalarLogicalProperty* value
        = static_cast<GOScalarLogicalProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    if (value->data()) {
        // clicking the selected button keeps it selected
        return;
    }
    value->data(true);
    value->setModified(true);
    GOUIPanel* buttonGroup = parentButtonGroup();
    if (buttonGroup) {
        buttonGroup->notifyChildSelected(findGraphicsObjectHandle(this), true);
    }
}
//=============================================================================
GOUIPanel*
GOUIRadioButton::parentButtonGroup()
{
    GOGObjectsProperty* parentProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (!parentProperty || parentProperty->data().empty()) {
        return nullptr;
    }
    GOUIPanel* panel
        = dynamic_cast<GOUIPanel*>(findGraphicsObject(parentProperty->data()[0], false));
    return (panel && panel->isButtonGroup()) ? panel : nullptr;
}
//=============================================================================
void
GOUIRadioButton::constructProperties()
{
    registerUIComponentCommonProperties(true, toggleStyle);
    registerProperty(new GOScalarLogicalProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOUITextVectorProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_WORD_WRAP_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_NONE_STR,
                         GO_PROPERTY_VALUE_TEX_STR, L"latex", L"html" }),
        GO_INTERPRETER_PROPERTY_NAME_STR);
    if (toggleStyle) {
        registerProperty(new GOArrayOfProperty, GO_ICON_PROPERTY_NAME_STR);
        registerProperty(
            new GORestrictedStringProperty({ GO_PROPERTY_VALUE_LEFT_STR,
                GO_PROPERTY_VALUE_RIGHT_STR, GO_PROPERTY_VALUE_CENTER_STR, L"leftmargin",
                L"rightmargin", GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
            GO_ICON_ALIGNMENT_PROPERTY_NAME_STR);
        registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
        registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_CENTER_STR,
                             GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    }
    sortProperties();
}
//=============================================================================
void
GOUIRadioButton::setupDefaults()
{
    setupUIComponentCommonDefaults(
        10, toggleStyle ? 10 : 10, toggleStyle ? 100 : 91, 22, true, toggleStyle);
    GOStringVectorProperty* text
        = static_cast<GOStringVectorProperty*>(findProperty(GO_TEXT_PROPERTY_NAME_STR));
    text->data(toggleStyle ? L"Toggle Button" : L"Radio Button");
    setRestrictedStringDefault(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    if (toggleStyle) {
        GOArrayOfProperty* icon
            = static_cast<GOArrayOfProperty*>(findProperty(GO_ICON_PROPERTY_NAME_STR));
        icon->data(ArrayOf::characterArray(L""));
        setRestrictedStringDefault(GO_ICON_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
        setRestrictedStringDefault(
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
        setRestrictedStringDefault(
            GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    }
}
//=============================================================================
void
GOUIRadioButton::setSelected(bool selected)
{
    GOScalarLogicalProperty* value
        = static_cast<GOScalarLogicalProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(selected);
    value->setModified(true);
    updateState();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
