//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUISwitch.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "UIItemsHelpers.h"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUISwitch::GOUISwitch(UISwitchStyle style) : switchStyle(style)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUISwitch::getType()
{
    switch (switchStyle) {
    case UISwitchStyle::Rocker:
        return GO_PROPERTY_VALUE_UIROCKERSWITCH_STR;
    case UISwitchStyle::Toggle:
        return GO_PROPERTY_VALUE_UITOGGLESWITCH_STR;
    case UISwitchStyle::Slider:
    default:
        return GO_PROPERTY_VALUE_UISWITCH_STR;
    }
}
//=============================================================================
std::string
GOUISwitch::getClassName()
{
    switch (switchStyle) {
    case UISwitchStyle::Rocker:
        return GO_CLASS_NAME_UI_ROCKERSWITCH_STR;
    case UISwitchStyle::Toggle:
        return GO_CLASS_NAME_UI_TOGGLESWITCH_STR;
    case UISwitchStyle::Slider:
    default:
        return GO_CLASS_NAME_UI_SWITCH_STR;
    }
}
//=============================================================================
void
GOUISwitch::updateState()
{
    if (hasChanged({ GO_ITEMS_PROPERTY_NAME_STR, GO_ITEMS_DATA_PROPERTY_NAME_STR,
            GO_VALUE_PROPERTY_NAME_STR })) {
        synchronizeValueWithItems();
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUISwitch::constructProperties()
{
    registerUIComponentCommonProperties(true, false);
    registerProperty(new GOUITextVectorProperty(true, false, true), GO_ITEMS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ITEMS_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_VALUE_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(
                         { GO_PROPERTY_VALUE_HORIZONTAL_STR, GO_PROPERTY_VALUE_VERTICAL_STR }),
        GO_ORIENTATION_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUISwitch::setupDefaults()
{
    bool horizontal = (switchStyle == UISwitchStyle::Slider);
    setupUIComponentCommonDefaults(
        100, 100, horizontal ? 45 : 20, horizontal ? 20 : 45, true, false);
    GOStringVectorProperty* items
        = static_cast<GOStringVectorProperty*>(findProperty(GO_ITEMS_PROPERTY_NAME_STR));
    wstringVector defaultItems = { L"Off", L"On" };
    items->data(defaultItems);
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(ArrayOf::characterArray(L"Off"));
    setScalarDoubleDefault(GO_VALUE_INDEX_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR,
        horizontal ? GO_PROPERTY_VALUE_HORIZONTAL_STR : GO_PROPERTY_VALUE_VERTICAL_STR);
}
//=============================================================================
void
GOUISwitch::synchronizeValueWithItems()
{
    wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
    ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
    GOArrayOfProperty* valueProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    GOScalarProperty* valueIndexProperty
        = static_cast<GOScalarProperty*>(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));

    int index = uiItemsIndexOfValue(valueProperty->data(), items, itemsData);
    if (index == 0) {
        if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
            Error(L"Nelson:uiswitch:invalidValue",
                L"'Value' must be an element of 'Items' or 'ItemsData'.");
        }
        index = items.empty() ? 0 : 1;
        valueProperty->data(uiItemsValueAtIndex(index, items, itemsData));
    }
    valueIndexProperty->data((double)index);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
