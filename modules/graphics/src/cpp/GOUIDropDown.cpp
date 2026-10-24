//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIDropDown.h"
#include "UIItemsHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOChartDecorators.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIDropDown::GOUIDropDown()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIDropDown::getType()
{
    return GO_PROPERTY_VALUE_UIDROPDOWN_STR;
}
//=============================================================================
std::string
GOUIDropDown::getClassName()
{
    return GO_CLASS_NAME_UI_DROPDOWN_STR;
}
//=============================================================================
void
GOUIDropDown::updateState()
{
    if (hasChanged({ GO_ITEMS_PROPERTY_NAME_STR, GO_ITEMS_DATA_PROPERTY_NAME_STR,
            GO_VALUE_PROPERTY_NAME_STR, GO_VALUE_INDEX_PROPERTY_NAME_STR,
            GO_EDITABLE_PROPERTY_NAME_STR })) {
        synchronizeValueWithItems();
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUIDropDown::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOUITextVectorProperty(true, false, true), GO_ITEMS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ITEMS_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_VALUE_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_PLACEHOLDER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DROP_DOWN_OPENING_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStyleConfigurationsProperty, GO_STYLE_CONFIGURATIONS_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUIDropDown::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 22);
    GOStringVectorProperty* items
        = static_cast<GOStringVectorProperty*>(findProperty(GO_ITEMS_PROPERTY_NAME_STR));
    items->data({ L"Option 1", L"Option 2", L"Option 3", L"Option 4" });
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(ArrayOf::characterArray(L"Option 1"));
    lastValidValue = ArrayOf::characterArray(L"Option 1");
    setScalarDoubleDefault(GO_VALUE_INDEX_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_PLACEHOLDER_PROPERTY_NAME_STR, L"");
}
//=============================================================================
void
GOUIDropDown::synchronizeValueWithItems()
{
    wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
    ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
    GOArrayOfProperty* valueProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    GOScalarProperty* valueIndexProperty
        = static_cast<GOScalarProperty*>(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));

    if (hasChanged(GO_VALUE_INDEX_PROPERTY_NAME_STR) && !hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
        int index = (int)valueIndexProperty->data();
        if (index < 1 || (size_t)index > items.size()) {
            valueIndexProperty->data((double)uiItemsIndexOfValue(lastValidValue, items, itemsData));
            valueIndexProperty->clearModified();
            Error(L"Nelson:uidropdown:invalidValueIndex",
                L"'ValueIndex' must be a valid index into 'Items'.");
        }
        valueProperty->data(uiItemsValueAtIndex(index, items, itemsData));
        lastValidValue = valueProperty->data();
        return;
    }
    ArrayOf value = valueProperty->data();
    int index = uiItemsIndexOfValue(value, items, itemsData);
    if (index == 0) {
        bool editable = stringCheck(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
        bool isText = value.isRowVectorCharacterArray() || value.isScalarStringArray();
        if (hasChanged(GO_VALUE_PROPERTY_NAME_STR) && !(editable && isText)) {
            valueProperty->data(lastValidValue);
            valueProperty->clearModified();
            Error(L"Nelson:uidropdown:invalidValue",
                L"'Value' must be an element of 'Items' or 'ItemsData'.");
        }
        if (!hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
            // Items changed: reset the selection to the first entry
            index = items.empty() ? 0 : 1;
            valueProperty->data(uiItemsValueAtIndex(index, items, itemsData));
        }
    }
    valueIndexProperty->data((double)index);
    lastValidValue = valueProperty->data();
}
//=============================================================================
void
GOUIDropDown::onActivated(int index)
{
    if (isProgrammaticChange()) {
        return;
    }
    wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
    ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
    GOArrayOfProperty* valueProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    GOScalarProperty* valueIndexProperty
        = static_cast<GOScalarProperty*>(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));

    ArrayOf previousValue = valueProperty->data();
    double previousIndex = valueIndexProperty->data();
    int newIndex = index + 1;
    ArrayOf newValue = uiItemsValueAtIndex(newIndex, items, itemsData);
    valueProperty->data(newValue);
    valueIndexProperty->data((double)newIndex);
    lastValidValue = newValue;

    wstringVector fieldNames
        = { L"Value", L"PreviousValue", L"Edited", L"ValueIndex", L"PreviousValueIndex" };
    ArrayOfVector fieldValues;
    fieldValues << newValue;
    fieldValues << previousValue;
    fieldValues << ArrayOf::logicalScalar(false);
    fieldValues << ArrayOf::doubleScalar((double)newIndex);
    fieldValues << ArrayOf::doubleScalar(previousIndex);
    emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData", L"ValueChanged",
        fieldNames, fieldValues);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
