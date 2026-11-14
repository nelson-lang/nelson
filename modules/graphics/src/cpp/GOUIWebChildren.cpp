//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIWebChildren.hpp"
#include "GOAxis.hpp"
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOUIComponent.h"
#include "GOUIComponentUnits.hpp"
#include "GOUIControl.h"
#include "GOUITab.h"
#include "GOUITabGroup.h"
#include "GOPropertyValues.hpp"
#include "RenderWeb.hpp"
#include "SizeChangedDispatcher.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
ownSizeInPixels(GOUIComponent* component, double& width, double& height);
static void
resizeTabPages(GOUIComponent* tabGroup);
//=============================================================================
bool
uiContainerHasAxesChild(GraphicsObject* container)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        container->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return false;
    }
    for (int64 childHandle : childProperty->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        if (dynamic_cast<GOAxis*>(child) != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
void
uiDescribeChildrenForWeb(
    GraphicsObject* container, RenderWeb& renderer, const std::vector<double>& position)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        container->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return;
    }
    for (int64 childHandle : childProperty->data()) {
        GraphicsObject* child = findGraphicsObject(childHandle, false);
        auto* component = dynamic_cast<GOUIComponent*>(child);
        auto* control = dynamic_cast<GOUIControl*>(child);
        if (component == nullptr && control == nullptr) {
            continue;
        }
        std::vector<double> childPosition
            = child->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (childPosition.size() != 4) {
            continue;
        }
        uiComponentPositionInParentFrame(child, childPosition, position[2], position[3]);
        childPosition[0] += position[0];
        childPosition[1] += position[1];
        if (component != nullptr) {
            component->describeForWeb(renderer, childPosition);
        } else {
            control->describeForWeb(renderer, childPosition);
        }
    }
}
//=============================================================================
void
uiNotifyWebContainerGeometry(GOUIComponent* container)
{
    if (container == nullptr) {
        return;
    }
    const std::wstring type = container->getType();
    if (type == GO_PROPERTY_VALUE_UIGRIDLAYOUT_STR) {
        // Lays its children out, then notifies them.
        container->updateState();
        return;
    }
    auto* tabPage = dynamic_cast<GOUITab*>(container);
    if (type == GO_PROPERTY_VALUE_UITABGROUP_STR) {
        resizeTabPages(container);
    } else if (tabPage != nullptr) {
        // A tab reports the page its group gives it.
        tabPage->onPageResized(0, 0);
    }
    bool resized = false;
    if (tabPage == nullptr && container->haveProperty(GO_SIZE_CHANGED_FCN_PROPERTY_NAME_STR)) {
        double width = 0;
        double height = 0;
        if (ownSizeInPixels(container, width, height)) {
            resized = recordContainerPixelSize(container, width, height);
        }
    }
    // A uifigure container also reports a pure move.
    const bool moved = consumeContainerMove(container);
    uiNotifyWebChildContainers(container);
    if (resized || moved) {
        notifySizeChanged(container);
    }
}
//=============================================================================
void
uiNotifyWebChildContainers(GraphicsObject* parent)
{
    auto* childProperty = parent
        ? dynamic_cast<GOGObjectsProperty*>(
              parent->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false))
        : nullptr;
    if (childProperty == nullptr) {
        return;
    }
    // A callback can delete a sibling: walk a copy and look each handle up again.
    const std::vector<int64> handles = childProperty->data();
    for (int64 childHandle : handles) {
        uiNotifyWebContainerGeometry(
            dynamic_cast<GOUIComponent*>(findGraphicsObject(childHandle, false)));
    }
}
//=============================================================================
void
uiNotifyWebContainerSizes(GOFigure* figure)
{
    GOFigureHost* host = figure ? figure->getHost() : nullptr;
    if (host == nullptr || !host->isHeadlessHost()) {
        return;
    }
    uiNotifyWebChildContainers(figure);
}
//=============================================================================
bool
ownSizeInPixels(GOUIComponent* component, double& width, double& height)
{
    std::vector<double> position
        = component->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() != 4) {
        return false;
    }
    double parentWidth = 0;
    double parentHeight = 0;
    uiComponentParentSizeInPixels(component, parentWidth, parentHeight);
    uiComponentPositionInPixels(component, position, parentWidth, parentHeight);
    width = position[2];
    height = position[3];
    return true;
}
//=============================================================================
// Sizes every page of a tab group, selected or not, as the native bindings do.
void
resizeTabPages(GOUIComponent* tabGroup)
{
    auto* childProperty = dynamic_cast<GOGObjectsProperty*>(
        tabGroup->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (childProperty == nullptr) {
        return;
    }
    const std::vector<int64> handles = childProperty->data();
    for (int64 childHandle : handles) {
        auto* tabPage = dynamic_cast<GOUITab*>(findGraphicsObject(childHandle, false));
        if (tabPage != nullptr) {
            tabPage->onPageResized(0, 0);
        }
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
