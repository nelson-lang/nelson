//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOSurfaceScale.hpp"
#include "GOFigure.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOUIComponentUnits.hpp"
#include "GOUIContainer.h"
#include "UnitsConversionHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
isInsideUIContainer(GraphicsObject* object);
//=============================================================================
void
logicalToSurfaceScale(GraphicsObject* object, double surfaceWidth, double surfaceHeight,
    double& scaleX, double& scaleY)
{
    scaleX = 1.0;
    scaleY = 1.0;
    if (object == nullptr || isInsideUIContainer(object)) {
        return;
    }
    GOFigure* fig = object->getParentFigure();
    if (fig == nullptr) {
        return;
    }
    double layoutWidth = fig->getLayoutWidth();
    double layoutHeight = fig->getLayoutHeight();
    if (layoutWidth <= 0 || layoutHeight <= 0 || surfaceWidth <= 0 || surfaceHeight <= 0) {
        return;
    }
    scaleX = surfaceWidth / layoutWidth;
    scaleY = surfaceHeight / layoutHeight;
}
//=============================================================================
std::vector<double>
positionOnSurface(GraphicsObject* object, const std::wstring& units,
    const std::vector<double>& position, double pixelOrigin, double surfaceWidth,
    double surfaceHeight)
{
    if (position.size() < 4) {
        return { 0, 0, 0, 0 };
    }
    double scaleX, scaleY;
    logicalToSurfaceScale(object, surfaceWidth, surfaceHeight, scaleX, scaleY);
    double logicalWidth = surfaceWidth / scaleX;
    double logicalHeight = surfaceHeight / scaleY;
    auto [x, y] = convertToPixels(units, position[0], position[1], logicalWidth, logicalHeight);
    auto [w, h] = convertToPixels(units, position[2], position[3], logicalWidth, logicalHeight);
    if (units == GO_PROPERTY_VALUE_PIXELS_STR) {
        x -= pixelOrigin;
        y -= pixelOrigin;
    }
    return { x * scaleX, y * scaleY, w * scaleX, h * scaleY };
}
//=============================================================================
std::pair<double, double>
offsetOnSurface(GraphicsObject* object, const std::wstring& units, double x, double y,
    double frameWidth, double frameHeight, double surfaceWidth, double surfaceHeight)
{
    double scaleX, scaleY;
    logicalToSurfaceScale(object, surfaceWidth, surfaceHeight, scaleX, scaleY);
    auto [px, py] = convertToPixels(units, x, y, frameWidth / scaleX, frameHeight / scaleY);
    return { px * scaleX, py * scaleY };
}
//=============================================================================
bool
axesParentLogicalSizeInPixels(GraphicsObject* axis, double& width, double& height)
{
    GOFigure* fig = axis ? axis->getParentFigure() : nullptr;
    if (fig == nullptr || !graphicsParentSizeInPixels(axis, fig, width, height)) {
        return false;
    }
    // A container reports its logical size; the figure reports its render
    // size, which is in device pixels on a scaled display.
    double scaleX, scaleY;
    logicalToSurfaceScale(axis, fig->getWidth(), fig->getHeight(), scaleX, scaleY);
    width /= scaleX;
    height /= scaleY;
    return true;
}
//=============================================================================
static bool
isInsideUIContainer(GraphicsObject* object)
{
    GraphicsObject* current = object;
    for (int guard = 0; current != nullptr && guard < 64; ++guard) {
        auto* parent = dynamic_cast<GOGObjectsProperty*>(
            current->findProperty(GO_PARENT_PROPERTY_NAME_STR, false));
        if (parent == nullptr || parent->data().empty()
            || parent->data()[0] < HANDLE_OFFSET_OBJECT) {
            return false;
        }
        current = findGraphicsObject(parent->data()[0], false);
        if (dynamic_cast<GOUIContainer*>(current) != nullptr) {
            return true;
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
