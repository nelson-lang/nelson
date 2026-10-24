//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIControl.h"
#include "GOFigure.hpp"
#include "RenderInterface.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
#include "GOStringVectorProperty.hpp"
#include "GOHelpers.hpp"
#include "GOCallbackProperty.hpp"
#include "RenderWeb.hpp"
#include "UnitsConversionHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::string
cssColor(const std::vector<double>& color, const char* fallback)
{
    if (color.size() < 3) {
        return fallback;
    }
    return "rgb(" + std::to_string(static_cast<int>(color[0] * 255)) + ","
        + std::to_string(static_cast<int>(color[1] * 255)) + ","
        + std::to_string(static_cast<int>(color[2] * 255)) + ")";
}
//=============================================================================
// Describes the control so the web frontend can build a real HTML widget for it. A blitted
// snapshot of the Qt widget cannot be clicked and ties the web path to Qt.
void
GOUIControl::describeForWeb(RenderWeb& renderer, const std::vector<double>& position)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    WebUIControl control;
    control.handle = static_cast<long long>(findGraphicsObjectHandle(this));
    control.style = wstring_to_utf8(findStringProperty(GO_STYLE_PROPERTY_NAME_STR));
    control.tooltip = wstring_to_utf8(findStringProperty(GO_TOOLTIP_PROPERTY_NAME_STR));
    auto* unitsProperty
        = dynamic_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
    if (unitsProperty != nullptr) {
        control.units = wstring_to_utf8(unitsProperty->data());
    }
    control.x = position[0];
    control.w = position[2];
    control.h = position[3];
    // Nelson positions controls from the bottom-left; the page uses top-left.
    double layoutHeight = renderer.height();
    if (control.units == "pixels") {
        GOFigure* figure = parentGoWindow ? parentGoWindow->hostFigure() : nullptr;
        if (figure != nullptr) {
            std::vector<double> figurePosition
                = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            if (figurePosition.size() >= 4) {
                layoutHeight = figurePosition[3];
            }
        }
    }
    control.y = layoutHeight - position[1] - position[3];
    // 'inactive' keeps the control looking normal (enabled) but non-interactive;
    // only 'off' greys it out.
    control.enabled = !stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    control.inactive = stringCheck(GO_ENABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_INACTIVE_STR);
    control.value = findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
    control.minimum = findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR);
    control.maximum = findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR);
    control.fontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
    control.background
        = cssColor(findVectorDoubleProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR), "");
    control.foreground
        = cssColor(findVectorDoubleProperty(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR), "");

    // String is a GOStringVectorProperty: a single label for most styles and the
    // entry list for the menu-like ones. Read it directly (findStringProperty on
    // a vector property is not valid).
    GOStringVectorProperty* stringProp
        = dynamic_cast<GOStringVectorProperty*>(findProperty(GO_STRING_PROPERTY_NAME_STR, false));
    if (stringProp != nullptr) {
        const wstringVector& lines = stringProp->data();
        if (lines.size() > 1) {
            for (const std::wstring& item : lines) {
                control.items.push_back(wstring_to_utf8(item));
            }
        } else if (lines.size() == 1) {
            control.text = wstring_to_utf8(lines[0]);
            control.items.push_back(control.text);
        }
    }
    renderer.addControl(control);
}
//=============================================================================
void
GOUIControl::applyWebAction(double value, const std::string& text)
{
    const std::wstring style = findStringProperty(GO_STYLE_PROPERTY_NAME_STR);
    bool managedByGroup = false;
    // Mirror the state produced in the browser onto the object: user callbacks
    // read these properties, and a pushbutton carries no state of its own.
    if (style == GO_PROPERTY_VALUE_EDIT_STR) {
        setStringDefault(GO_STRING_PROPERTY_NAME_STR, utf8_to_wstring(text));
    } else if (style != GO_PROPERTY_VALUE_PUSHBUTTON_STR && style != GO_PROPERTY_VALUE_TEXT_STR) {
        findProperty(GO_VALUE_PROPERTY_NAME_STR)->set(ArrayOf::doubleScalar(value));
        findProperty(GO_VALUE_PROPERTY_NAME_STR)->setModified(true);
        managedByGroup = onGroupButtonClicked();
    }
    int64 thisHandle = findGraphicsObjectHandle(this);
    if (thisHandle != -1) {
        setCurrentObject(thisHandle);
    }
    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)findProperty(GO_CALLBACK_PROPERTY_NAME_STR);
    if (goCallback && !managedByGroup) {
        // Enqueue on the CallbackQueue like the Qt widgets do. The web terminal returns from
        // getLine when the queue is non-empty, so the CLI REPL drains it through
        // Evaluator::processCliCallbacks, the same path adv-cli uses.
        goCallback->pushEvent(this, L"ActionData", L"Action");
    }
}
//=============================================================================
void
GOUIControl::paintMe(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::wstring renderName = gc.getRenderName();
    std::vector<double> sizevec(findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (sizevec.size() < 4) {
        return;
    }
    GOUnitsProperty* unitsProperty = (GOUnitsProperty*)findProperty(GO_UNITS_PROPERTY_NAME_STR);
    std::wstring currentUnits = unitsProperty->data();
    if (currentUnits != GO_PROPERTY_VALUE_PIXELS_STR) {
        auto [pixelX, pixelY]
            = convertToPixels(currentUnits, sizevec[0], sizevec[1], gc.width(), gc.height());
        auto [pixelW, pixelH]
            = convertToPixels(currentUnits, sizevec[2], sizevec[3], gc.width(), gc.height());
        sizevec[0] = pixelX;
        sizevec[1] = pixelY;
        sizevec[2] = pixelW;
        sizevec[3] = pixelH;
    }
    if (renderName == L"Web") {
        auto* web = dynamic_cast<RenderWeb*>(&gc);
        if (web != nullptr) {
            describeForWeb(*web, sizevec);
        }
        return;
    }
    if (binding && binding->exists()) {
        if (renderName != L"GL" && renderName != L"Image" && renderName != L"SVG") {
            binding->renderSnapshot(gc);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
