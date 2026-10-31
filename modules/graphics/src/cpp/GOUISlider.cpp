//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "GOUISlider.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "UITicksHelpers.h"
#include "Warning.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUISlider::GOUISlider(bool isRangeSlider) : rangeStyle(isRangeSlider)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
void
GOUISlider::updateState()
{
    enforceFixedThickness();
    if (uiTicksSyncModes(this)) {
        updateAutoTicks();
    }
    GOUIComponent::updateState();
}
//=============================================================================
void
GOUISlider::enforceFixedThickness()
{
    if (!hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        return;
    }
    std::vector<double> position(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (position.size() != 4) {
        return;
    }
    const bool vertical
        = stringCheck(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
    const size_t fixedIndex = vertical ? 2 : 3; // width for a vertical slider, else height
    const double fixedThickness = 3.0;
    if (std::fabs(position[fixedIndex] - fixedThickness) < 1e-9) {
        return;
    }
    if (vertical) {
        RaiseWarning(
            L"Nelson:ui:Slider:fixedWidth", _W("The width of this component cannot be changed."));
    } else {
        RaiseWarning(
            L"Nelson:ui:Slider:fixedHeight", _W("The height of this component cannot be changed."));
    }
    position[fixedIndex] = fixedThickness;
    setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    findProperty(GO_POSITION_PROPERTY_NAME_STR)->setModified(true);
}
//=============================================================================
std::wstring
GOUISlider::getType()
{
    return rangeStyle ? GO_PROPERTY_VALUE_UIRANGESLIDER_STR : GO_PROPERTY_VALUE_UISLIDER_STR;
}
//=============================================================================
std::string
GOUISlider::getClassName()
{
    return rangeStyle ? GO_CLASS_NAME_UI_RANGESLIDER_STR : GO_CLASS_NAME_UI_SLIDER_STR;
}
//=============================================================================
void
GOUISlider::constructProperties()
{
    registerUIComponentCommonProperties(true, false);
    if (rangeStyle) {
        registerProperty(new GOTwoVectorProperty, GO_VALUE_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GOScalarProperty, GO_VALUE_PROPERTY_NAME_STR);
    }
    registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty(
                         { GO_PROPERTY_VALUE_HORIZONTAL_STR, GO_PROPERTY_VALUE_VERTICAL_STR }),
        GO_ORIENTATION_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_STEP_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_MAJOR_TICKS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR);
    registerProperty(
        new GOUITextVectorProperty(true, false, true), GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_MINOR_TICKS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUISlider::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 150, 3, true, false);
    if (rangeStyle) {
        setTwoVectorDefault(GO_VALUE_PROPERTY_NAME_STR, 0, 100);
    } else {
        setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
    }
    setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, 0, 100);
    setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR);
    setScalarDoubleDefault(GO_STEP_PROPERTY_NAME_STR, 0.1);
    setRestrictedStringDefault(GO_STEP_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    updateAutoTicks();
}
//=============================================================================
void
GOUISlider::updateAutoTicks()
{
    uiTicksUpdateAuto(this, true);
}
//=============================================================================
void
GOUISlider::onUserValueChanging(double value, double highValue)
{
    if (isProgrammaticChange()) {
        return;
    }
    ArrayOf liveValue = rangeStyle ? ArrayOf::doubleRowVector({ value, highValue })
                                   : ArrayOf::doubleScalar(value);
    wstringVector fieldNames = { L"Value" };
    ArrayOfVector fieldValues;
    fieldValues << liveValue;
    emitCallbackEvent(GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR, L"ValueChangingData",
        L"ValueChanging", fieldNames, fieldValues);
}
//=============================================================================
void
GOUISlider::onUserValueChanged(double value, double highValue)
{
    if (isProgrammaticChange()) {
        return;
    }
    ArrayOf previousValue;
    ArrayOf newValue;
    if (rangeStyle) {
        GOTwoVectorProperty* valueProperty
            = static_cast<GOTwoVectorProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
        previousValue = valueProperty->get();
        valueProperty->data({ value, highValue });
        newValue = valueProperty->get();
    } else {
        GOScalarProperty* valueProperty
            = static_cast<GOScalarProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
        previousValue = ArrayOf::doubleScalar(valueProperty->data());
        valueProperty->data(value);
        newValue = ArrayOf::doubleScalar(value);
    }
    wstringVector fieldNames = { L"Value", L"PreviousValue" };
    ArrayOfVector fieldValues;
    fieldValues << newValue;
    fieldValues << previousValue;
    emitCallbackEvent(GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR, L"ValueChangedData", L"ValueChanged",
        fieldNames, fieldValues);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
