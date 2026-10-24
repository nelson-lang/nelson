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
#include "GOUISpinner.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUISpinner::GOUISpinner()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUISpinner::getType()
{
    return GO_PROPERTY_VALUE_UISPINNER_STR;
}
//=============================================================================
std::string
GOUISpinner::getClassName()
{
    return GO_CLASS_NAME_UI_SPINNER_STR;
}
//=============================================================================
void
GOUISpinner::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_LOWER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_UPPER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ROUND_FRACTIONAL_VALUES_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_ALLOW_EMPTY_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_PLACEHOLDER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUISpinner::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 22);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    value->data(ArrayOf::doubleScalar(0));
    setScalarDoubleDefault(GO_STEP_PROPERTY_NAME_STR, 1);
    setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, -std::numeric_limits<double>::infinity(),
        std::numeric_limits<double>::infinity());
    setRestrictedStringDefault(
        GO_LOWER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_UPPER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_ROUND_FRACTIONAL_VALUES_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR, L"%11.4g");
    setRestrictedStringDefault(GO_ALLOW_EMPTY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
    setStringDefault(GO_PLACEHOLDER_PROPERTY_NAME_STR, L"");
}
//=============================================================================
std::wstring
GOUISpinner::formatValue(double value)
{
    std::wstring format = findStringProperty(GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR);
    if (format.empty()) {
        format = L"%11.4g";
    }
    wchar_t buffer[256];
    swprintf(buffer, 256, format.c_str(), value);
    std::wstring text(buffer);
    size_t start = text.find_first_not_of(L' ');
    if (start != std::wstring::npos) {
        text = text.substr(start);
    }
    return text;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
