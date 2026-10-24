//=============================================================================
// Copyright (c) 2026-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <cmath>
#include <vector>
#include "GOGObjectsProperty.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUIComponent.h"
#include "GOUnitsProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
inline GOUIComponent*
uiComponentWebParentComponent(GOUIComponent* component)
{
    if (component == nullptr) {
        return nullptr;
    }
    auto* parentProperty = dynamic_cast<GOGObjectsProperty*>(
        component->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
    if (parentProperty == nullptr || parentProperty->data().empty()) {
        return nullptr;
    }
    return dynamic_cast<GOUIComponent*>(findGraphicsObject(parentProperty->data()[0], false));
}
//=============================================================================
inline bool
uiComponentWebSpansDevicePixelFigure(
    GOUIComponent* component, const std::vector<double>& figurePosition, double devicePixelRatio)
{
    if (component == nullptr || figurePosition.size() < 4 || figurePosition[2] <= 0
        || figurePosition[3] <= 0) {
        return false;
    }
    auto* unitsProperty = dynamic_cast<GOUnitsProperty*>(
        component->findProperty(GO_UNITS_PROPERTY_NAME_STR, false));
    if (unitsProperty != nullptr && unitsProperty->data() != GO_PROPERTY_VALUE_PIXELS_STR) {
        return false;
    }
    std::vector<double> position
        = component->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() != 4) {
        return false;
    }
    double widthRatio = position[2] / figurePosition[2];
    double heightRatio = position[3] / figurePosition[3];
    return std::abs(widthRatio - devicePixelRatio) <= 0.05
        && std::abs(heightRatio - devicePixelRatio) <= 0.05;
}
//=============================================================================
inline bool
uiComponentWebInDevicePixelSubtree(
    GOUIComponent* component, const std::vector<double>& figurePosition, double devicePixelRatio)
{
    for (GOUIComponent* current = component; current != nullptr;
        current = uiComponentWebParentComponent(current)) {
        if (uiComponentWebSpansDevicePixelFigure(current, figurePosition, devicePixelRatio)) {
            return true;
        }
    }
    return false;
}
//=============================================================================
inline double
uiComponentWebPositionScale(
    GOUIComponent* component, GOFigure* figure, double rendererWidth, double rendererHeight)
{
    if (figure == nullptr) {
        return 1.0;
    }
    std::vector<double> figurePosition
        = figure->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (figurePosition.size() < 4) {
        return 1.0;
    }
    double devicePixelRatio
        = figure->findScalarDoubleProperty(GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR);
    bool rendererUsesLogicalPixels = std::abs(rendererWidth - figurePosition[2]) <= 1.0
        && std::abs(rendererHeight - figurePosition[3]) <= 1.0;
    if (rendererUsesLogicalPixels && devicePixelRatio > 0 && std::abs(devicePixelRatio - 1.0) > 1e-6
        && uiComponentWebInDevicePixelSubtree(component, figurePosition, devicePixelRatio)) {
        return 1.0 / devicePixelRatio;
    }
    return 1.0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
