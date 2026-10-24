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
#include "GOUIContainer.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOFontUnitsProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOHelpers.hpp"
#include "GOUIComponentUnits.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Containers without font properties (uitabgroup) still write labels: size
// them like the default UI font.
static constexpr double DEFAULT_CHROME_FONT_PIXEL_SIZE = 12.0;
//=============================================================================
std::vector<int64>
GOUIContainer::getChildrenHandles()
{
    GOGObjectsProperty* childrenProperty
        = static_cast<GOGObjectsProperty*>(findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!childrenProperty) {
        return {};
    }
    return childrenProperty->data();
}
//=============================================================================
void
GOUIContainer::refreshChildrenGeometry()
{
    for (int64 childHandle : getChildrenHandles()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (!child) {
            continue;
        }
        GOGenericProperty* positionProperty
            = child->findProperty(GO_POSITION_PROPERTY_NAME_STR, false);
        if (positionProperty) {
            positionProperty->setModified(true);
        }
        child->updateState();
    }
}
//=============================================================================
void
GOUIContainer::onGeometryChanged()
{
    refreshChildrenGeometry();
    if (haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)) {
        emitCallbackEvent(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR, L"SizeChangedData", L"SizeChanged",
            {}, {}, true);
    }
}
//=============================================================================
void
GOUIContainer::paintMe(RenderInterface& gc)
{
    GOUIComponent::paintMe(gc);
    paintContainer(gc);
}
//=============================================================================
void
GOUIContainer::paintContainer(RenderInterface& gc)
{
    if (stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return;
    }
    std::vector<double> frame = frameRect(gc);
    if (frame.size() != 4 || !gc.pushRegion(frame[0], frame[1], frame[2], frame[3])) {
        // The backend paints containers on its own (on screen the container
        // widget covers the canvas and draws its children itself).
        return;
    }
    paintChrome(gc);
    std::vector<double> content = contentRect(gc);
    if (content.size() == 4 && gc.pushRegion(content[0], content[1], content[2], content[3])) {
        paintContent(gc);
        gc.popRegion();
    }
    gc.popRegion();
}
//=============================================================================
void
GOUIContainer::paintChrome(RenderInterface& gc)
{
    auto* background = dynamic_cast<GOColorProperty*>(
        findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
    if (background != nullptr && !background->isNone()) {
        gc.clear(background->data());
    }
}
//=============================================================================
void
GOUIContainer::paintContent(RenderInterface& gc)
{
    for (int64 childHandle : getChildrenHandles()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (child == nullptr) {
            continue;
        }
        if (auto* container = dynamic_cast<GOUIContainer*>(child)) {
            updateStateIfChanged(container);
            container->paintContainer(gc);
            continue;
        }
        if (dynamic_cast<GOUIComponent*>(child) != nullptr
            || child->getType() == GO_PROPERTY_VALUE_UICONTROL_STR) {
            // Widgets are composited by the toolkit (Qt) or emitted as controls
            // (web); only the graphics objects are drawn here.
            continue;
        }
        updateStateIfChanged(child);
        child->paintMe(gc);
    }
}
//=============================================================================
void
GOUIContainer::updateStateIfChanged(GraphicsObject* child)
{
    if (child->hasAnyChanged()) {
        child->updateState();
    }
}
//=============================================================================
void
GOUIContainer::paintContentInFrame(RenderInterface& gc)
{
    std::vector<double> content = contentRect(gc);
    if (content.size() != 4 || !gc.pushRegion(content[0], content[1], content[2], content[3])) {
        return;
    }
    paintContent(gc);
    gc.popRegion();
}
//=============================================================================
std::vector<double>
GOUIContainer::frameRect(RenderInterface& gc)
{
    std::vector<double> rect = findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (rect.size() != 4) {
        return {};
    }
    uiComponentPositionInPixels(this, rect, gc.width(), gc.height());
    return rect;
}
//=============================================================================
std::vector<double>
GOUIContainer::contentRect(RenderInterface& gc)
{
    return { 0, 0, gc.width(), gc.height() };
}
//=============================================================================
FontSpec
GOUIContainer::chromeFont()
{
    FontSpec font;
    if (haveProperty(GO_FONT_NAME_PROPERTY_NAME_STR)) {
        font.family = findStringProperty(GO_FONT_NAME_PROPERTY_NAME_STR);
    }
    if (haveProperty(GO_FONT_SIZE_PROPERTY_NAME_STR)) {
        double fontSize = findScalarDoubleProperty(GO_FONT_SIZE_PROPERTY_NAME_STR);
        if (fontSize > 0) {
            font.pixelSize = fontSize;
        }
    } else {
        font.pixelSize = DEFAULT_CHROME_FONT_PIXEL_SIZE;
    }
    if (haveProperty(GO_FONT_WEIGHT_PROPERTY_NAME_STR)
        && stringCheck(GO_FONT_WEIGHT_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_BOLD_STR)) {
        font.setWeight(FontSpec::WeightBold);
    }
    if (haveProperty(GO_FONT_ANGLE_PROPERTY_NAME_STR)
        && stringCheck(GO_FONT_ANGLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ITALIC_STR)) {
        font.italic = true;
    }
    return font;
}
//=============================================================================
double
GOUIContainer::chromeTextHeight(RenderInterface& gc)
{
    int width = 0;
    int height = 0;
    int xoffset = 0;
    int yoffset = 0;
    gc.measureText(L"Ag", chromeFont(), RenderInterface::Min, RenderInterface::Min, width, height,
        xoffset, yoffset);
    return height > 0 ? (double)height : DEFAULT_CHROME_FONT_PIXEL_SIZE;
}
//=============================================================================
void
GOUIContainer::registerPanelLikeProperties()
{
    registerUIComponentCommonProperties(true, true, true);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GORestrictedStringProperty({ L"lefttop", L"centertop", L"righttop" }),
        GO_TITLE_POSITION_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_BORDER_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR);
    registerProperty(
        new GORestrictedStringProperty({ GO_PROPERTY_VALUE_LINE_STR, GO_PROPERTY_VALUE_NONE_STR }),
        GO_BORDER_TYPE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty(0.0, (double)std::numeric_limits<int>::max()),
        GO_BORDER_WIDTH_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_CLIPPING_PROPERTY_NAME_STR);
    registerProperty(new GOFontUnitsProperty, GO_FONT_UNITS_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SCROLLABLE_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOUIContainer::setupPanelLikeDefaults(double x, double y, double w, double h)
{
    setupUIComponentCommonDefaults(x, y, w, h);
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setRestrictedStringDefault(GO_TITLE_POSITION_PROPERTY_NAME_STR, L"lefttop");
    setThreeVectorDefault(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 0.129411764705882,
        0.129411764705882, 0.129411764705882);
    setThreeVectorDefault(
        GO_BORDER_COLOR_PROPERTY_NAME_STR, 0.490196078431373, 0.490196078431373, 0.490196078431373);
    setThreeVectorDefault(GO_HIGHLIGHT_COLOR_PROPERTY_NAME_STR, 0.490196078431373,
        0.490196078431373, 0.490196078431373);
    setRestrictedStringDefault(GO_BORDER_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_LINE_STR);
    setScalarDoubleDefault(GO_BORDER_WIDTH_PROPERTY_NAME_STR, 1);
    setRestrictedStringDefault(GO_CLIPPING_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setStringDefault(GO_FONT_UNITS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_POINTS_STR);
    setScalarDoubleDefault(GO_FONT_SIZE_PROPERTY_NAME_STR, 8);
    setRestrictedStringDefault(
        GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SCROLLABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
