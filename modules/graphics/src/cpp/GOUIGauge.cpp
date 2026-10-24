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
#include "GOUIGauge.h"
#include "UITicksHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOColorProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringAutoManualProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOVectorTwoDoubleProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOUIGauge::GOUIGauge(UIGaugeStyle style) : gaugeStyle(style)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUIGauge::getType()
{
    switch (gaugeStyle) {
    case UIGaugeStyle::Linear:
        return GO_PROPERTY_VALUE_UILINEARGAUGE_STR;
    case UIGaugeStyle::NinetyDegree:
        return GO_PROPERTY_VALUE_UININETYDEGREEGAUGE_STR;
    case UIGaugeStyle::Semicircular:
        return GO_PROPERTY_VALUE_UISEMICIRCULARGAUGE_STR;
    case UIGaugeStyle::Circular:
    default:
        return GO_PROPERTY_VALUE_UIGAUGE_STR;
    }
}
//=============================================================================
std::string
GOUIGauge::getClassName()
{
    switch (gaugeStyle) {
    case UIGaugeStyle::Linear:
        return GO_CLASS_NAME_UI_LINEARGAUGE_STR;
    case UIGaugeStyle::NinetyDegree:
        return GO_CLASS_NAME_UI_NINETYDEGREEGAUGE_STR;
    case UIGaugeStyle::Semicircular:
        return GO_CLASS_NAME_UI_SEMICIRCULARGAUGE_STR;
    case UIGaugeStyle::Circular:
    default:
        return GO_CLASS_NAME_UI_GAUGE_STR;
    }
}
//=============================================================================
void
GOUIGauge::updateState()
{
    if (uiTicksSyncModes(this)) {
        uiTicksUpdateAuto(this, false, autoMajorIntervals());
    }
    GOUIComponent::updateState();
}
//=============================================================================
int
GOUIGauge::autoMajorIntervals() const
{
    // the quarter-circle scale is too short for more than two major intervals
    return (gaugeStyle == UIGaugeStyle::NinetyDegree) ? 2 : 5;
}
//=============================================================================
void
GOUIGauge::constructProperties()
{
    registerUIComponentCommonProperties(true, true);
    registerProperty(new GOScalarProperty, GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOTwoVectorProperty, GO_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_SCALE_COLORS_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_SCALE_COLOR_LIMITS_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty, GO_MAJOR_TICKS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR);
    registerProperty(
        new GOUITextVectorProperty(true, false, true), GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR);
    registerProperty(new GOVectorProperty(false, true), GO_MINOR_TICKS_PROPERTY_NAME_STR);
    registerProperty(new GOAutoManualProperty, GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR);
    if (gaugeStyle == UIGaugeStyle::Linear) {
        registerProperty(new GORestrictedStringProperty(
                             { GO_PROPERTY_VALUE_HORIZONTAL_STR, GO_PROPERTY_VALUE_VERTICAL_STR }),
            GO_ORIENTATION_PROPERTY_NAME_STR);
    } else {
        registerProperty(new GORestrictedStringProperty({ GO_PROPERTY_VALUE_CLOCKWISE_STR,
                             GO_PROPERTY_VALUE_COUNTERCLOCKWISE_STR }),
            GO_SCALE_DIRECTION_PROPERTY_NAME_STR);
        if (gaugeStyle == UIGaugeStyle::NinetyDegree) {
            registerProperty(new GORestrictedStringProperty(
                                 { L"northwest", L"northeast", L"southwest", L"southeast" }),
                GO_ORIENTATION_PROPERTY_NAME_STR);
        } else if (gaugeStyle == UIGaugeStyle::Semicircular) {
            registerProperty(new GORestrictedStringProperty(
                                 { GO_PROPERTY_VALUE_NORTH_STR, GO_PROPERTY_VALUE_SOUTH_STR,
                                     GO_PROPERTY_VALUE_EAST_STR, GO_PROPERTY_VALUE_WEST_STR }),
                GO_ORIENTATION_PROPERTY_NAME_STR);
        }
    }
    sortProperties();
}
//=============================================================================
void
GOUIGauge::setupDefaults()
{
    double width = 120;
    double height = 120;
    switch (gaugeStyle) {
    case UIGaugeStyle::Linear:
        width = 120;
        height = 40;
        break;
    case UIGaugeStyle::NinetyDegree:
        width = 90;
        height = 90;
        break;
    case UIGaugeStyle::Semicircular:
        width = 120;
        height = 65;
        break;
    default:
        break;
    }
    setupUIComponentCommonDefaults(100, 100, width, height, true, true);
    setThreeVectorDefault(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
    setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
    setTwoVectorDefault(GO_LIMITS_PROPERTY_NAME_STR, 0, 100);
    setRestrictedStringDefault(GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(
        GO_MAJOR_TICK_LABELS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    setRestrictedStringDefault(GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
    static_cast<GOVectorProperty*>(findProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR))
        ->data(std::vector<double>());
    uiTicksUpdateAuto(this, false, autoMajorIntervals());
    if (gaugeStyle == UIGaugeStyle::Linear) {
        setRestrictedStringDefault(
            GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_HORIZONTAL_STR);
    } else {
        setRestrictedStringDefault(
            GO_SCALE_DIRECTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CLOCKWISE_STR);
        if (gaugeStyle == UIGaugeStyle::NinetyDegree) {
            setRestrictedStringDefault(GO_ORIENTATION_PROPERTY_NAME_STR, L"northwest");
        } else if (gaugeStyle == UIGaugeStyle::Semicircular) {
            setRestrictedStringDefault(
                GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORTH_STR);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
