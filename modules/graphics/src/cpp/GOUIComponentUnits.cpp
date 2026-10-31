//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOUIComponentUnits.hpp"
#include "GOUIComponent.h"
#include "GOUIContainer.h"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPositionHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUnitsProperty.hpp"
#include "RenderWeb.hpp"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GraphicsObject*
uiComponentParentObject(GraphicsObject* component)
{
    if (component == nullptr) {
        return nullptr;
    }
    auto* parentProperty = dynamic_cast<GOGObjectsProperty*>(
        component->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parentProperty == nullptr || parentProperty->data().empty()) {
        return nullptr;
    }
    int64 parentHandle = parentProperty->data()[0];
    if (parentHandle < HANDLE_OFFSET_OBJECT) {
        return nullptr;
    }
    return findGraphicsObject(parentHandle, false);
}
//=============================================================================
static bool
uiComponentFigureSizeInPixels(GOUIComponent* component, double& width, double& height)
{
    GOFigureHost* host = component ? component->getParentFigureHost() : nullptr;
    GOFigure* figure = host ? host->hostFigure() : nullptr;
    width = figure ? figure->getLayoutWidth() : 0;
    height = figure ? figure->getLayoutHeight() : 0;
    if ((width <= 0 || height <= 0) && host) {
        double frameX = 0;
        double frameY = 0;
        double frameTop = 0;
        host->hostFrameGeometry(frameX, frameY, width, height, frameTop);
    }
    if ((width <= 0 || height <= 0) && figure) {
        std::vector<double> position
            = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        if (position.size() == 4) {
            width = position[2];
            height = position[3];
        }
    }
    return width > 0 && height > 0;
}
//=============================================================================
static bool
uiComponentOwnSizeInPixels(GOUIComponent* component, double& width, double& height)
{
    if (component == nullptr) {
        return false;
    }
    std::vector<double> position
        = component->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() != 4) {
        return false;
    }
    width = position[2];
    height = position[3];
    auto* unitsProperty = dynamic_cast<GOUnitsProperty*>(
        component->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
    if (unitsProperty == nullptr || unitsProperty->data() == GO_PROPERTY_VALUE_PIXELS_STR) {
        return width > 0 && height > 0;
    }
    double parentWidth = 0;
    double parentHeight = 0;
    uiComponentParentSizeInPixels(component, parentWidth, parentHeight);
    auto [pixelW, pixelH]
        = convertToPixels(unitsProperty->data(), width, height, parentWidth, parentHeight);
    width = pixelW;
    height = pixelH;
    return width > 0 && height > 0;
}
//=============================================================================
bool
uiParentContentRect(GraphicsObject* object, std::vector<double>& content, double& frameHeight)
{
    auto* parentComponent = dynamic_cast<GOUIComponent*>(uiComponentParentObject(object));
    double frameWidth = 0;
    frameHeight = 0;
    if (!uiComponentOwnSizeInPixels(parentComponent, frameWidth, frameHeight)) {
        return false;
    }
    content = { 0, 0, frameWidth, frameHeight };
    // The container chrome (border, title band, tab bar) is deducted exactly as
    // it is painted: contentRect only measures text, a Qt-free context is enough.
    if (auto* container = dynamic_cast<GOUIContainer*>(parentComponent)) {
        RenderWeb gc(0, 0, frameWidth, frameHeight);
        std::vector<double> rect = container->contentRect(gc);
        if (rect.size() == 4) {
            content = rect;
        }
    }
    return true;
}
//=============================================================================
bool
graphicsParentSizeInPixels(GraphicsObject* object, GOFigure* figure, double& width, double& height)
{
    // Units of an object held by a ui container (uipanel, uitab, ...) are
    // relative to that container content area, not to the figure.
    std::vector<double> content;
    double frameHeight = 0;
    if (uiParentContentRect(object, content, frameHeight)) {
        width = content[2];
        height = content[3];
        return true;
    }
    width = figure ? figure->getWidth() : 0;
    height = figure ? figure->getHeight() : 0;
    return width > 0 && height > 0;
}
//=============================================================================
bool
uiComponentParentSizeInPixels(GOUIComponent* component, double& width, double& height)
{
    if (dynamic_cast<GOUIComponent*>(uiComponentParentObject(component)) == nullptr) {
        return uiComponentFigureSizeInPixels(component, width, height);
    }
    std::vector<double> content;
    double frameHeight = 0;
    if (!uiParentContentRect(component, content, frameHeight)) {
        return false;
    }
    width = content[2];
    height = content[3];
    return width > 0 && height > 0;
}
//=============================================================================
void
uiComponentPositionInPixels(
    GraphicsObject* component, std::vector<double>& position, double width, double height)
{
    if (component == nullptr || position.size() != 4) {
        return;
    }
    auto* unitsProperty = dynamic_cast<GOUnitsProperty*>(
        component->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
    if (unitsProperty == nullptr || unitsProperty->data() == GO_PROPERTY_VALUE_PIXELS_STR) {
        return;
    }
    auto [pixelX, pixelY]
        = convertToPixels(unitsProperty->data(), position[0], position[1], width, height);
    auto [pixelW, pixelH]
        = convertToPixels(unitsProperty->data(), position[2], position[3], width, height);
    position[0] = pixelX;
    position[1] = pixelY;
    position[2] = pixelW;
    position[3] = pixelH;
}
//=============================================================================
void
GOUIComponent::onUnitsChanged(bool newWidget)
{
    if (!haveProperty(GO_UNITS_PROPERTY_NAME_STR)
        || (!hasChanged(GO_UNITS_PROPERTY_NAME_STR) && !newWidget)) {
        return;
    }
    bool positionChanged = hasChanged(GO_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)
        || hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
    if (!positionChanged) {
        auto* unitsProperty
            = static_cast<GOUnitsProperty*>(findProperty(GO_UNITS_PROPERTY_NAME_STR));
        double width = 0;
        double height = 0;
        uiComponentParentSizeInPixels(this, width, height);
        syncPositionUnitsTriplet(
            this, unitsProperty->getPreviousUnits(), unitsProperty->data(), width, height);
        clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    } else {
        reconcilePositionPixelOrigin(this);
    }
    clearChanged(GO_UNITS_PROPERTY_NAME_STR);
}
//=============================================================================
void
uiComponentPositionInParentFrame(
    GraphicsObject* component, std::vector<double>& position, double frameWidth, double frameHeight)
{
    std::vector<double> content = { 0, 0, frameWidth, frameHeight };
    double contentFrameHeight = 0;
    uiParentContentRect(component, content, contentFrameHeight);
    uiComponentPositionInPixels(component, position, content[2], content[3]);
    if (position.size() == 4) {
        position[0] += content[0];
        position[1] += content[1];
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
