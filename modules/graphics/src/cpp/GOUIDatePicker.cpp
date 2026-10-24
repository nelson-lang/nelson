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
#include "GOUIDatePicker.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
makeDatetime(double serialValue)
{
    wstringVector fieldnames = { L"SerialValue", L"FormatValue", L"TimeZoneValue" };
    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::doubleScalar(serialValue);
    fieldvalues << ArrayOf::characterArray(L"dd-MMM-yyyy");
    fieldvalues << ArrayOf::characterArray(L"");
    return ArrayOf::classArray(L"datetime", fieldnames, fieldvalues);
}
//=============================================================================
GOUIDatePicker::GOUIDatePicker()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIDatePicker::getType()
{
    return GO_PROPERTY_VALUE_UIDATEPICKER_STR;
}
//=============================================================================
std::string
GOUIDatePicker::getClassName()
{
    return GO_CLASS_NAME_UI_DATEPICKER_STR;
}
//=============================================================================
void
GOUIDatePicker::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_DISPLAY_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISABLED_DATES_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_DISABLED_DAYS_OF_WEEK_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_PLACEHOLDER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUIDatePicker::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 150, 22);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(makeDatetime(std::nan(""))); // NaT
    setStringDefault(GO_DISPLAY_FORMAT_PROPERTY_NAME_STR, L"dd-MMM-uuuu");
    GOArrayOfProperty* disabledDates
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DISABLED_DATES_PROPERTY_NAME_STR));
    disabledDates->data(ArrayOf::empty(0, 0));
    GOArrayOfProperty* disabledDays
        = static_cast<GOArrayOfProperty*>(findProperty(GO_DISABLED_DAYS_OF_WEEK_PROPERTY_NAME_STR));
    disabledDays->data(ArrayOf::empty(0, 0));
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_PLACEHOLDER_PROPERTY_NAME_STR, L"");
}
//=============================================================================
void
GOUIDatePicker::onUserDateChanged(double serialDate)
{
    if (isProgrammaticChange()) {
        return;
    }
    GOArrayOfProperty* valueProperty
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    ArrayOf previousValue = valueProperty->data();
    ArrayOf newValue = makeDatetime(serialDate);
    valueProperty->data(newValue);
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
