//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIListBox.h"
#include "UIItemsHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOChartDecorators.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOVectorProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIListBox::GOUIListBox()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIListBox::getType()
{
    return GO_PROPERTY_VALUE_UILISTBOX_STR;
}
//=============================================================================
std::string
GOUIListBox::getClassName()
{
    return GO_CLASS_NAME_UI_LISTBOX_STR;
}
//=============================================================================
void
GOUIListBox::updateState()
{
    if (hasChanged({ GO_ITEMS_PROPERTY_NAME_STR, GO_ITEMS_DATA_PROPERTY_NAME_STR,
            GO_VALUE_PROPERTY_NAME_STR, GO_MULTISELECT_PROPERTY_NAME_STR })) {
        synchronizeValueWithItems();
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUIListBox::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOUITextVectorProperty(true, false, true), GO_ITEMS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_ITEMS_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_VALUE_INDEX_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_MULTISELECT_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DOUBLE_CLICKED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOStyleConfigurationsProperty, GO_STYLE_CONFIGURATIONS_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUIListBox::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 74);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    GOStringVectorProperty* items
        = static_cast<GOStringVectorProperty*>(findProperty(GO_ITEMS_PROPERTY_NAME_STR));
    items->data({ L"Item 1", L"Item 2", L"Item 3", L"Item 4" });
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(ArrayOf::characterArray(L"Item 1"));
    lastValidValue = ArrayOf::characterArray(L"Item 1");
    GOArrayOfProperty* valueIndex
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));
    valueIndex->data(ArrayOf::doubleScalar(1));
    setRestrictedStringDefault(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
std::vector<int>
GOUIListBox::currentIndicesFromProperties()
{
    std::vector<int> indices;
    ArrayOf valueIndex = findArrayOfProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR);
    if (valueIndex.isEmpty() || !valueIndex.isNumeric()) {
        return indices;
    }
    ArrayOf asDouble(valueIndex);
    asDouble.promoteType(NLS_DOUBLE);
    const double* values = (const double*)asDouble.dataPointer();
    for (indexType k = 0; k < valueIndex.elementCount(); ++k) {
        indices.push_back((int)values[k]);
    }
    return indices;
}
//=============================================================================
void
GOUIListBox::synchronizeValueWithItems()
{
    wstringVector items = findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
    ArrayOf itemsData = findArrayOfProperty(GO_ITEMS_DATA_PROPERTY_NAME_STR);
    GOArrayOfProperty* valueProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    GOArrayOfProperty* valueIndexProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_INDEX_PROPERTY_NAME_STR));
    bool multiselect = stringCheck(GO_MULTISELECT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);

    ArrayOf value = valueProperty->data();
    std::vector<int> indices;

    auto rejectValue = [&](const std::wstring& message) {
        valueProperty->data(lastValidValue);
        valueProperty->clearModified();
        Error(L"Nelson:graphics:invalidListBoxValue", message);
    };
    if (value.isCell()) {
        const ArrayOf* elements = (const ArrayOf*)value.dataPointer();
        for (indexType k = 0; k < value.elementCount(); ++k) {
            int index = uiItemsIndexOfValue(elements[k], items, itemsData);
            if (index == 0) {
                if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                    rejectValue(resolveMessageTemplate(L"Nelson:graphics:elementsItemsItemsdata",
                        L"'Value' must be elements of 'Items' or 'ItemsData'."));
                }
                continue;
            }
            indices.push_back(index);
        }
        if (!multiselect && indices.size() > 1) {
            rejectValue(
                Nelson::resolveMessageTemplate(L"Nelson:graphics:multipleRequireMultiselectSet",
                    L"Multiple values require 'Multiselect' set to 'on'."));
        }
    } else if (!value.isEmpty()) {
        int index = uiItemsIndexOfValue(value, items, itemsData);
        if (index == 0) {
            if (hasChanged(GO_VALUE_PROPERTY_NAME_STR)) {
                rejectValue(Nelson::resolveMessageTemplate(L"Nelson:graphics:elementItemsItemsdata",
                    L"'Value' must be an element of 'Items' or 'ItemsData'."));
            }
            // Items changed: reset selection to the first entry
            if (!items.empty()) {
                valueProperty->data(uiItemsValueAtIndex(1, items, itemsData));
                indices.push_back(1);
            }
        } else {
            indices.push_back(index);
        }
    }
    if (indices.empty()) {
        valueIndexProperty->data(ArrayOf::empty(0, 0));
    } else if (indices.size() == 1) {
        valueIndexProperty->data(ArrayOf::doubleScalar(indices[0]));
    } else {
        std::vector<double> asDouble(indices.begin(), indices.end());
        valueIndexProperty->data(ArrayOf::doubleRowVector(asDouble));
    }
    lastValidValue = valueProperty->data();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
