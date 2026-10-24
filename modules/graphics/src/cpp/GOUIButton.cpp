//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIButton.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarLogicalProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringVectorProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIButton::GOUIButton(bool isStateButton) : stateStyle(isStateButton)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIButton::getType()
{
    return stateStyle ? GO_PROPERTY_VALUE_UISTATEBUTTON_STR : GO_PROPERTY_VALUE_UIBUTTON_STR;
}
//=============================================================================
std::string
GOUIButton::getClassName()
{
    return stateStyle ? GO_CLASS_NAME_UI_STATE_BUTTON_STR : GO_CLASS_NAME_UI_BUTTON_STR;
}
//=============================================================================
void
GOUIButton::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOUITextVectorProperty, GO_TEXT_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_NONE_STR,
                         GO_PROPERTY_VALUE_TEX_STR, L"latex", L"html" }),
        GO_INTERPRETER_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_WORD_WRAP_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ICON_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_LEFT_STR,
                         GO_PROPERTY_VALUE_RIGHT_STR, GO_PROPERTY_VALUE_CENTER_STR, L"leftmargin",
                         L"rightmargin", GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
        GO_ICON_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_CENTER_STR,
                         GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR }),
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR);
    if (stateStyle) {
        registerProperty(new GOScalarLogicalProperty, GO_VALUE_PROPERTY_NAME_STR);
        registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GOCallbackProperty, GO_BUTTON_PUSHED_FCN_PROPERTY_NAME_STR);
    }
    sortProperties();
}
//=============================================================================
void
GOUIButton::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 22);
    GOStringVectorProperty* text
        = static_cast<GOStringVectorProperty*>(findProperty(GO_TEXT_PROPERTY_NAME_STR));
    text->data(stateStyle ? L"State Button" : L"Button");
    GOArrayOfProperty* icon
        = static_cast<GOArrayOfProperty*>(findProperty(GO_ICON_PROPERTY_NAME_STR));
    icon->data(ArrayOf::characterArray(L""));
    setRestrictedStringDefault(GO_INTERPRETER_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR);
    setRestrictedStringDefault(GO_WORD_WRAP_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_ICON_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    setRestrictedStringDefault(
        GO_VERTICAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
