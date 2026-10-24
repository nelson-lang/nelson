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
#include <cstdio>
#include "GOUIEditField.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOFixedVectorProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIEditField::GOUIEditField(bool isNumeric) : numericStyle(isNumeric)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIEditField::getType()
{
    return numericStyle ? GO_PROPERTY_VALUE_UINUMERICEDITFIELD_STR
                        : GO_PROPERTY_VALUE_UIEDITFIELD_STR;
}
//=============================================================================
std::string
GOUIEditField::getClassName()
{
    return numericStyle ? GO_CLASS_NAME_UI_NUMERICEDITFIELD_STR : GO_CLASS_NAME_UI_EDITFIELD_STR;
}
//=============================================================================
void
GOUIEditField::constructProperties()
{
    registerUIComponentCommonProperties();
    registerProperty(new GOArrayOfProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_EDITABLE_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_PLACEHOLDER_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_VALUE_CHANGED_FCN_PROPERTY_NAME_STR);
    if (numericStyle) {
        registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
        registerProperty(new GOOnOffProperty, GO_LOWER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR);
        registerProperty(new GOOnOffProperty, GO_UPPER_LIMIT_INCLUSIVE_PROPERTY_NAME_STR);
        registerProperty(new GOOnOffProperty, GO_ROUND_FRACTIONAL_VALUES_PROPERTY_NAME_STR);
        registerProperty(new GOStringProperty, GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR);
        registerProperty(new GOOnOffProperty, GO_ALLOW_EMPTY_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GOTwoVectorProperty, GO_CHARACTER_LIMITS_PROPERTY_NAME_STR);
        registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_TEXT_STR, L"letters",
                             L"digits", L"alphanumerics" }),
            GO_INPUT_TYPE_PROPERTY_NAME_STR);
        registerProperty(new GOCallbackProperty, GO_VALUE_CHANGING_FCN_PROPERTY_NAME_STR);
    }
    sortProperties();
}
//=============================================================================
void
GOUIEditField::setupDefaults()
{
    setupUIComponentCommonDefaults(100, 100, 100, 22);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    GOArrayOfProperty* value
        = static_cast<GOArrayOfProperty*>(findProperty(GO_VALUE_PROPERTY_NAME_STR));
    if (numericStyle) {
        value->data(ArrayOf::doubleScalar(0));
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
        setRestrictedStringDefault(
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_RIGHT_STR);
    } else {
        value->data(ArrayOf::characterArray(L""));
        setTwoVectorDefault(
            GO_CHARACTER_LIMITS_PROPERTY_NAME_STR, 0, std::numeric_limits<double>::infinity());
        setRestrictedStringDefault(GO_INPUT_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TEXT_STR);
        setRestrictedStringDefault(
            GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LEFT_STR);
    }
    setRestrictedStringDefault(GO_EDITABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_PLACEHOLDER_PROPERTY_NAME_STR, L"");
}
//=============================================================================
std::wstring
GOUIEditField::formatNumericValue(double value)
{
    std::wstring format = findStringProperty(GO_VALUE_DISPLAY_FORMAT_PROPERTY_NAME_STR);
    if (format.empty()) {
        format = L"%11.4g";
    }
    wchar_t buffer[256];
    swprintf(buffer, 256, format.c_str(), value);
    std::wstring text(buffer);
    // trim leading spaces produced by width specifiers
    size_t start = text.find_first_not_of(L' ');
    if (start != std::wstring::npos) {
        text = text.substr(start);
    }
    return text;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
