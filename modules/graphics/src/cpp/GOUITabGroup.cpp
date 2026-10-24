//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUITabGroup.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GORestrictedStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOHelpers.hpp"
#include "GOUITab.h"
#include "RenderInterface.hpp"
#include "RenderWeb.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Tab bar metrics and colors: a label is its text plus this padding on every
// side, drawn on a bar slightly darker than the page it opens.
static constexpr double TAB_LABEL_PADDING = 6.0;
static const std::vector<double> TAB_BAR_COLOR { 0.878, 0.878, 0.878 };
static const std::vector<double> TAB_BORDER_COLOR { 0.690, 0.690, 0.690 };
static const std::vector<double> TAB_LABEL_COLOR { 0.129, 0.129, 0.129 };
//=============================================================================
static std::vector<GOUITab*>
tabsOf(GOUITabGroup* group);
//=============================================================================
static std::vector<double>
tabPageColor(GOUITab* tab);
//=============================================================================
GOUITabGroup::GOUITabGroup()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUITabGroup::getType()
{
    return GO_PROPERTY_VALUE_UITABGROUP_STR;
}
//=============================================================================
std::string
GOUITabGroup::getClassName()
{
    return GO_CLASS_NAME_UI_TABGROUP_STR;
}
//=============================================================================
void
GOUITabGroup::constructProperties()
{
    registerUIComponentCommonProperties(false, false, true);
    registerProperty(
        new GORestrictedStringProperty({ GO_PROPERTY_VALUE_TOP_STR, GO_PROPERTY_VALUE_BOTTOM_STR,
            GO_PROPERTY_VALUE_LEFT_STR, GO_PROPERTY_VALUE_RIGHT_STR }),
        GO_TAB_LOCATION_PROPERTY_NAME_STR);
    registerProperty(new GOGObjectsProperty, GO_SELECTED_TAB_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SELECTION_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUITabGroup::setupDefaults()
{
    setupUIComponentCommonDefaults(20, 20, 250, 210, false, false);
    setRestrictedStringDefault(GO_TAB_LOCATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_TOP_STR);
    setRestrictedStringDefault(
        GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
double
GOUITabGroup::tabBarThickness(RenderInterface& gc)
{
    if (tabsOf(this).empty()) {
        return 0.0;
    }
    return chromeTextHeight(gc) + TAB_LABEL_PADDING;
}
//=============================================================================
std::vector<double>
GOUITabGroup::contentRect(RenderInterface& gc)
{
    const double thickness = tabBarThickness(gc);
    const std::wstring location = findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR);
    double width = gc.width();
    double height = gc.height();
    double x = 0;
    double y = 0;
    if (location == GO_PROPERTY_VALUE_LEFT_STR) {
        x = thickness;
        width -= thickness;
    } else if (location == GO_PROPERTY_VALUE_RIGHT_STR) {
        width -= thickness;
    } else if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
        y = thickness;
        height -= thickness;
    } else {
        height -= thickness;
    }
    if (width <= 0 || height <= 0) {
        return {};
    }
    return { x, y, width, height };
}
//=============================================================================
void
GOUITabGroup::paintChrome(RenderInterface& gc)
{
    // The WebFigureBridge emits the tab header as live HTML controls. Painting
    // the native header into the canvas as well creates a second, displaced
    // row (and clips the live row when the group touches the canvas edge).
    // Native Qt rendering keeps the existing chrome path below.
    if (dynamic_cast<RenderWeb*>(&gc) != nullptr) {
        return;
    }
    const std::vector<GOUITab*> tabs = tabsOf(this);
    if (tabs.empty()) {
        return;
    }
    const std::vector<double> page = contentRect(gc);
    if (page.size() != 4) {
        return;
    }
    const double thickness = tabBarThickness(gc);
    const FontSpec font = chromeFont();
    GOUITab* selected = selectedTab();
    gc.setupDirectDraw();
    gc.setLineStyle(L"-");
    gc.lineWidth(1.0);
    // The bar runs along the side named by TabLocation, over the whole side;
    // each label gets the width of its own text, the selected one standing out
    // on the page color.
    const std::vector<double> bar = tabBarRect(gc, page, thickness);
    gc.color(TAB_BAR_COLOR);
    gc.rectFill(bar[0], bar[1], bar[0] + bar[2], bar[1] + bar[3]);
    const bool verticalBar = isVerticalTabBar();
    double cursor = 0;
    for (GOUITab* tab : tabs) {
        const std::wstring title = tab->findStringProperty(GO_TITLE_PROPERTY_NAME_STR);
        int textWidth = 0;
        int textHeight = 0;
        int xoffset = 0;
        int yoffset = 0;
        gc.measureText(title, font, RenderInterface::Min, RenderInterface::Min, textWidth,
            textHeight, xoffset, yoffset);
        const double extent
            = (verticalBar ? (double)textHeight : (double)textWidth) + 2 * TAB_LABEL_PADDING;
        std::vector<double> box = tabLabelBox(page, thickness, cursor, extent);
        const bool isSelected = (tab == selected);
        gc.color(isSelected ? tabPageColor(tab) : TAB_BAR_COLOR);
        gc.rectFill(box[0], box[1], box[0] + box[2], box[1] + box[3]);
        gc.color(TAB_BORDER_COLOR);
        gc.rect(box[0], box[1], box[0] + box[2], box[1] + box[3]);
        gc.putText(box[0] + box[2] / 2.0, box[1] + box[3] / 2.0, title, TAB_LABEL_COLOR,
            RenderInterface::Mean, RenderInterface::Mean, font, verticalBar ? 90.0 : 0.0);
        cursor += extent;
    }
    gc.releaseDirectDraw();
}
//=============================================================================
void
GOUITabGroup::paintContent(RenderInterface& gc)
{
    // Only the tab on top is on screen; the others hold no pixels.
    GOUITab* selected = selectedTab();
    if (selected != nullptr) {
        updateStateIfChanged(selected);
        selected->paintContainer(gc);
    }
    if (tabsOf(this).empty()) {
        return;
    }
    // The page outline goes on last: the tab has just filled the page with
    // its own background color, which would swallow a line drawn earlier.
    gc.setupDirectDraw();
    gc.setLineStyle(L"-");
    gc.lineWidth(1.0);
    gc.color(TAB_BORDER_COLOR);
    gc.rect(0, 0, gc.width(), gc.height());
    gc.releaseDirectDraw();
}
//=============================================================================
GOUITab*
GOUITabGroup::selectedTab()
{
    const std::vector<GOUITab*> tabs = tabsOf(this);
    if (tabs.empty()) {
        return nullptr;
    }
    auto* selectedProperty
        = dynamic_cast<GOGObjectsProperty*>(findProperty(GO_SELECTED_TAB_PROPERTY_NAME_STR, false));
    if (selectedProperty != nullptr && !selectedProperty->data().empty()) {
        GraphicsObject* object = findGraphicsObject(selectedProperty->data()[0], false);
        for (GOUITab* tab : tabs) {
            if (tab == object) {
                return tab;
            }
        }
    }
    return tabs.front();
}
//=============================================================================
bool
GOUITabGroup::isVerticalTabBar()
{
    const std::wstring location = findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR);
    return location == GO_PROPERTY_VALUE_LEFT_STR || location == GO_PROPERTY_VALUE_RIGHT_STR;
}
//=============================================================================
std::vector<double>
GOUITabGroup::tabLabelBox(
    const std::vector<double>& page, double thickness, double cursor, double extent)
{
    const std::wstring location = findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR);
    if (location == GO_PROPERTY_VALUE_LEFT_STR) {
        return { 0, page[1] + page[3] - cursor - extent, thickness, extent };
    }
    if (location == GO_PROPERTY_VALUE_RIGHT_STR) {
        return { page[2], page[1] + page[3] - cursor - extent, thickness, extent };
    }
    if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
        return { cursor, 0, extent, thickness };
    }
    return { cursor, page[1] + page[3], extent, thickness };
}
//=============================================================================
std::vector<double>
GOUITabGroup::tabBarRect(RenderInterface& gc, const std::vector<double>& page, double thickness)
{
    const std::wstring location = findStringProperty(GO_TAB_LOCATION_PROPERTY_NAME_STR);
    if (location == GO_PROPERTY_VALUE_LEFT_STR) {
        return { 0, 0, thickness, gc.height() };
    }
    if (location == GO_PROPERTY_VALUE_RIGHT_STR) {
        return { page[2], 0, thickness, gc.height() };
    }
    if (location == GO_PROPERTY_VALUE_BOTTOM_STR) {
        return { 0, 0, gc.width(), thickness };
    }
    return { 0, page[1] + page[3], gc.width(), thickness };
}
//=============================================================================
std::vector<GOUITab*>
tabsOf(GOUITabGroup* group)
{
    std::vector<GOUITab*> tabs;
    for (int64 childHandle : group->getChildrenHandles()) {
        if (auto* tab = dynamic_cast<GOUITab*>(findGraphicsObject(childHandle, false))) {
            tabs.push_back(tab);
        }
    }
    return tabs;
}
//=============================================================================
std::vector<double>
tabPageColor(GOUITab* tab)
{
    auto* background = dynamic_cast<GOColorProperty*>(
        tab->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
    if (background == nullptr || background->isNone()) {
        return { 1, 1, 1 };
    }
    return background->data();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
