//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <limits>
#include "GOUIControl.h"
#include "GOPropertyValues.hpp"
#include "GOPropertyNames.hpp"
#include "GOColorProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOFontAngleProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOFontWeightProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOEnableProperty.hpp"
#include "GOAlignHorizProperty.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOUnitsProperty.hpp"
#include "GOVectorProperty.hpp"
#include "GOControlStyleProperty.hpp"
#include "GOCallbackProperty.hpp"
#include "GOBusyActionProperty.hpp"
#include "GOFigure.hpp"
#include "UnitsConversionHelpers.hpp"
#include "GOCommonProperties.hpp"
#include "GOPositionHelpers.hpp"
#include "GOUIComponentUnits.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
GOUIControl::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOBusyActionProperty, GO_BUSY_ACTION_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_INTERRUPTIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CALLBACK_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_CREATE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_DELETE_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOFourVectorProperty, GO_POSITION_PROPERTY_NAME_STR);
    auto* stringProperty = new GOStringVectorProperty;
    stringProperty->setEmptyReturnsChar(true);
    registerProperty(stringProperty, GO_STRING_PROPERTY_NAME_STR);
    registerProperty(new GOControlStyleProperty, GO_STYLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOUnitsProperty, GO_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_C_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_BACKGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(1.0, (double)std::numeric_limits<int>::max()),
        GO_FONT_SIZE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_FONT_NAME_PROPERTY_NAME_STR);
    registerProperty(new GOFontAngleProperty, GO_FONT_ANGLE_PROPERTY_NAME_STR);
    registerProperty(new GOFontWeightProperty, GO_FONT_WEIGHT_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOAlignHorizProperty, GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR);
    registerProperty(new GOEnableProperty, GO_ENABLE_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TOOLTIP_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MIN_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_MAX_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(
                         std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max()),
        GO_VALUE_PROPERTY_NAME_STR);
    registerProperty(new GOFixedVectorProperty(2), GO_SLIDER_STEP_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, GO_LISTBOX_TOP_PROPERTY_NAME_STR);

    registerCommonProperties(this);
    sortProperties();
}
//=============================================================================
void
GOUIControl::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, getType());
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 20, 20, 60, 20);
    setStringDefault(GO_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PIXELS_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setThreeVectorDefault(
        GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, 245.0 / 255.0, 245.0 / 255.0, 245.0 / 255.0);
    setThreeVectorDefault(
        GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 33.0 / 255.0, 33.0 / 255.0, 33.0 / 255.0);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8);
    setStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
#ifdef _MSC_VER
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"MS Sans Serif");
#else
    setStringDefault(GO_FONT_NAME_PROPERTY_NAME_STR, L"Helvetica");
#endif
    setStringDefault(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setStringDefault(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NORMAL_STR);
    setRestrictedStringDefault(
        GO_HORIZONTAL_ALIGNMENT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_CENTER_STR);
    setRestrictedStringDefault(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setRestrictedStringDefault(GO_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PUSHBUTTON_STR);
    setStringDefault(GO_TOOLTIP_PROPERTY_NAME_STR, L"");
    setScalarDoubleDefault(GO_MIN_PROPERTY_NAME_STR, 0);
    setScalarDoubleDefault(GO_MAX_PROPERTY_NAME_STR, 1);
    setScalarDoubleDefault(GO_VALUE_PROPERTY_NAME_STR, 0);
    setTwoVectorDefault(GO_SLIDER_STEP_PROPERTY_NAME_STR, 0.0100, 0.1000);
    setScalarDoubleDefault(GO_LISTBOX_TOP_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_BUSY_ACTION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_QUEUE_STR);
    setRestrictedStringDefault(GO_INTERRUPTIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    initializeCommonProperties(this);
}
//=============================================================================
void
GOUIControl::onUnitsChanged(bool newWidget)
{
    bool needContinue = hasChanged(GO_UNITS_PROPERTY_NAME_STR) || newWidget;
    if (!needContinue) {
        return;
    }

    if (!hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        GOUnitsProperty* unitsProperty = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
        std::wstring previousUnits = unitsProperty->getPreviousUnits();
        std::wstring currentUnits = unitsProperty->data();

        GOFigure* figure = parentGoWindow ? parentGoWindow->hostFigure() : nullptr;
        double width = figure ? figure->getLayoutWidth() : 0;
        double height = figure ? figure->getLayoutHeight() : 0;
        if ((width <= 0 || height <= 0) && parentGoWindow) {
            double frameX = 0;
            double frameY = 0;
            double frameTop = 0;
            parentGoWindow->hostFrameGeometry(frameX, frameY, width, height, frameTop);
        }
        // held by a uipanel/uitab: relative to its content area
        std::vector<double> content;
        double frameHeight = 0;
        if (uiParentContentRect(this, content, frameHeight)) {
            width = content[2];
            height = content[3];
        }

        syncPositionUnitsTriplet(this, previousUnits, currentUnits, width, height);
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    }

    clearChanged(GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
