//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <utility>
#include <vector>
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Pixel and point positions of graphics objects are logical pixels. The
// figure canvas may be rendered at device resolution (a surface of
// surfaceWidth x surfaceHeight for a figure laid out in fewer logical
// pixels), while the content of a uipanel/uitab is painted on a surface
// already in logical pixels. Returns the logical-to-surface factors for the
// surface the object is painted on.
void
logicalToSurfaceScale(GraphicsObject* object, double surfaceWidth, double surfaceHeight,
    double& scaleX, double& scaleY);
//=============================================================================
// Position (x, y, width, height) given in units inside the frame of the whole
// paint surface, mapped on that surface. pixelOrigin is the pixel coordinate
// of the bottom-left corner of the frame: 1 for 1-based pixel positions.
std::vector<double>
positionOnSurface(GraphicsObject* object, const std::wstring& units,
    const std::vector<double>& position, double pixelOrigin, double surfaceWidth,
    double surfaceHeight);
//=============================================================================
// Offset (x, y) given in units inside a frame of frameWidth x frameHeight
// surface pixels, mapped on the paint surface.
std::pair<double, double>
offsetOnSurface(GraphicsObject* object, const std::wstring& units, double x, double y,
    double frameWidth, double frameHeight, double surfaceWidth, double surfaceHeight);
//=============================================================================
// Size of the frame the axes Units are relative to, in logical pixels (the
// figure layout or the container content area): the reference for the
// conversions of the axes position properties between Units.
bool
axesParentLogicalSizeInPixels(GraphicsObject* axis, double& width, double& height);
//=============================================================================
} // namespace Nelson
//=============================================================================
