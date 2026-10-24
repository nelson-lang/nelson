//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include "GOUIKnob.h"
#include "UIItemsHelpers.h"
#include "UITicksHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIKnob::GOUIKnob(bool isDiscrete) : discreteStyle(isDiscrete)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIKnob::getType()
{
    return discreteStyle ? GO_PROPERTY_VALUE_UIDISCRETEKNOB_STR : GO_PROPERTY_VALUE_UIKNOB_STR;
}
//=============================================================================
std::string
GOUIKnob::getClassName()
{
    return discreteStyle ? GO_CLASS_NAME_UI_DISCRETEKNOB_STR : GO_CLASS_NAME_UI_KNOB_STR;
}
//=============================================================================
void
GOUIKnob::updateState()
{
    if (discreteStyle
        && hasChanged({ GO_ITEMS_PROPERTY_NAME_STR, GO_ITEMS_DATA_PROPERTY_NAME_STR,
            GO_VALUE_PROPERTY_NAME_STR })) {
        syncDiscreteValueIndex();
    }
    if (!discreteStyle && uiTicksSyncModes(this)) {
        uiTicksUpdateAuto(this, false);
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUIKnob::constructProperties()
{
    registerUIComponentCommonProperties(true, false);
    if (discreteStyle) {
        registerProperty(new GOUITextVectorProperty(true, false, true), GO_ITEMS_PROPERTY_NAME_STR);
        registerProperty(new GOArrayOfProperty, GO_ITEMS_DATA_PROPERTY_NAME_STR);
        registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
        registerProperty(new GOScalarProperty, GO_VALUE_INDEX_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GOScalarProperty, GO_VALUE_PROPERTY_NAME_STR);
        registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
        registerProperty(new GOVectorProperty, GO_MAJOR_TICKS_PROPERTY_NAME_STR);
        registerProperty(new GOAutoManualProperty, GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR);
        registerProperty(
            new GOUITextVectorProperty(true, false, true), GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR);
        registerProperty(new GOAutoManualProperty, GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR);
        registerProperty(new GOVectorProperty(false, true), GO_MINOR_TICKS_PROPERTY_NAME_STR);
        registerProperty(new GOAutoManualProperty, GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR);
        registerProperty(new GOCallbackProperty, GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR);
    }
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUIKnob::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 60, 60, true, false);
    if (discreteStyle) {
        GOStringVectorProperty* items
            = static_cast<GOStringVectorProperty*>(findProperty(GO_ITEMS_PROPERTY_NAME_STR));
        items->data({ L"Off", L"Low", L"Medium", L"High" });
        GOArrayOfProperty* value
            = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
        value->data(ArrayOf::characterArray(L"Off"));
        setScalarDoubleDefault(GO_VALUE_INDEX_PROPERTY_NAME_STR, 1);
    } else {
        setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
        setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, 0, 100);
        setRestrictedStringDefault(
            GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setRestrictedStringDefault(
            GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        setRestrictedStringDefault(
            GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
        static_cast<GOVectorProperty*>(findProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR))
            ->data(std::vector<double>());
        uiTicksUpdateAuto(this, false);
    }
}
//=============================================================================
int
GOUIKnob::syncDiscreteValueIndex()
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
            Error(L"Nelson:uiknob:invalidValue",
                L"'Value' must be an element of 'Items' or 'ItemsData'.");
        }
        index = items.empty() ? 0 : 1;
        valueProperty->data(uiItemsValueAtIndex(index, items, itemsData));
    }
    valueIndexProperty->data((double)index);
    return index;
}
//=============================================================================
void
GOUIKnob::onUserValueChanging(double value)
{
    if (isProgrammaticChange()) {
        return;
    }
    wstringVector fieldNames = { L"Value" };
    ArrayOfVector fieldValues;
    fieldValues << ArrayOf::doubleScalar(value);
    emitCallbackEvent(GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR, L"ValueChangingData",
        L"ValueChanging", fieldNames, fieldValues);
}
//=============================================================================
void
GOUIKnob::onUserValueChanged(double value)
{
    if (isProgrammaticChange()) {
        return;
    }
    GOScalarProperty* valueProperty
        = static_cast<GOScalarProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    double previousValue = valueProperty->data();
    valueProperty->data(value);
    wstringVector fieldNames = { L"Value", L"PreviousValue" };
    ArrayOfVector fieldValues;
    fieldValues << ArrayOf::doubleScalar(value);
    fieldValues << ArrayOf::doubleScalar(previousValue);
    emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData", L"ValueChanged",
        fieldNames, fieldValues);
}
//=============================================================================
void
GOUIKnob::onUserDiscreteChanged(int index)
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
    if (newIndex == (int)previousIndex) {
        return;
    }
    ArrayOf newValue = uiItemsValueAtIndex(newIndex, items, itemsData);
    valueProperty->data(newValue);
    valueIndexProperty->data((double)newIndex);
    wstringVector fieldNames = { L"Value", L"PreviousValue", L"ValueIndex", L"PreviousValueIndex" };
    ArrayOfVector fieldValues;
    fieldValues << newValue;
    fieldValues << previousValue;
    fieldValues << ArrayOf::doubleScalar((double)newIndex);
    fieldValues << ArrayOf::doubleScalar(previousIndex);
    emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData", L"ValueChanged",
        fieldNames, fieldValues);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
