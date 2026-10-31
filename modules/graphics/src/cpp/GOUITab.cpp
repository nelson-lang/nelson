//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUITab.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOCallbackProperty.hpp"
#include "GOColorProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOUIComponent.h"
#include "GOUIComponentUnits.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOStringProperty.hpp"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
parentTabGroupSize(GOUITab* tab, double& width, double& height)
{
    auto* parentProperty
        = dynamic_cast<GOGObjectsProperty*>(tab->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parentProperty == nullptr || parentProperty->data().empty()) {
        return false;
    }
    auto* parentComponent
        = dynamic_cast<GOUIComponent*>(findGraphicsObject(parentProperty->data()[0], false));
    if (parentComponent == nullptr
        || parentComponent->getType() != GO_PROPERTY_VALUE_UITABGROUP_STR) {
        return false;
    }
    std::vector<double> parentPosition
        = parentComponent->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (parentPosition.size() != 4 || parentPosition[2] <= 0 || parentPosition[3] <= 0) {
        return false;
    }
    double parentParentWidth = 0;
    double parentParentHeight = 0;
    if (uiComponentParentSizeInPixels(parentComponent, parentParentWidth, parentParentHeight)) {
        uiComponentPositionInPixels(
            parentComponent, parentPosition, parentParentWidth, parentParentHeight);
    }
    width = parentPosition[2];
    height = parentPosition[3];
    return true;
}
//=============================================================================
GOUITab::GOUITab()
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
std::wstring
GOUITab::getType()
{
    return GO_PROPERTY_VALUE_UITAB_STR;
}
//=============================================================================
std::string
GOUITab::getClassName()
{
    return GO_CLASS_NAME_UI_TAB_STR;
}
//=============================================================================
void
GOUITab::constructProperties()
{
    registerUIComponentCommonProperties(false, true, true);
    registerProperty(new GOStringProperty, GO_TITLE_PROPERTY_NAME_STR);
    registerProperty(new GOColorProperty, GO_FOREGROUND_COLOR_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOCallbackProperty, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_SCROLLABLE_PROPERTY_NAME_STR);
    sortProperties();
}
//=============================================================================
void
GOUITab::setupDefaults()
{
    setupUIComponentCommonDefaults(1, 1, 250, 210, false, true);
    setStringDefault(GO_TITLE_PROPERTY_NAME_STR, L"");
    setThreeVectorDefault(GO_FOREGROUND_COLOR_PROPERTY_NAME_STR, 0.129411764705882,
        0.129411764705882, 0.129411764705882);
    setRestrictedStringDefault(
        GO_AUTO_RESIZE_CHILDREN_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_SCROLLABLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
std::vector<double>
GOUITab::frameRect(RenderInterface& gc)
{
    // The tab group has already installed the page area as the current frame;
    // the page fills it. The tab's own Position is page-relative ([1 1 w h]),
    // so it says nothing about where the page sits inside the group.
    return { 0, 0, gc.width(), gc.height() };
}
//=============================================================================
void
GOUITab::onPageResized(double width, double height)
{
    // Public tab geometry follows the group, independently of the native
    // page inset for its border and tab bar. Children use this logical area
    // for their bottom-left coordinate system on every backend.
    parentTabGroupSize(this, width, height);
    if (width <= 0 || height <= 0) {
        return;
    }
    setFourVectorDefault(GO_POSITION_PROPERTY_NAME_STR, 0, 0, width, height);
    setFourVectorDefault(GO_INNER_POSITION_PROPERTY_NAME_STR, 0, 0, width, height);
    setFourVectorDefault(GO_OUTER_POSITION_PROPERTY_NAME_STR, 0, 0, width, height);
    refreshChildrenGeometry();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
