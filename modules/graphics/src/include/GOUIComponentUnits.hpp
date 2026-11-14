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
#include <vector>
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIComponent;
class GOUIContainer;
class GOFigure;
class GraphicsObject;
//=============================================================================
// Area left to the children of `container` in a frame of frameWidth x
// frameHeight: {x, y, w, h}, bottom-left origin. A uitab yields its group's
// page (border and tab bar deducted); any other container its contentRect.
NLSGRAPHICS_IMPEXP bool
uiContainerChildContentRect(
    GOUIContainer* container, double frameWidth, double frameHeight, std::vector<double>& content);
//=============================================================================
NLSGRAPHICS_IMPEXP void
uiComponentPositionInPixels(
    GraphicsObject* component, std::vector<double>& position, double width, double height);
//=============================================================================
// Position of `component` in pixels of its parent frame (frameWidth x
// frameHeight), the chrome of a uipanel parent skipped.
NLSGRAPHICS_IMPEXP void
uiComponentPositionInParentFrame(GraphicsObject* component, std::vector<double>& position,
    double frameWidth, double frameHeight);
//=============================================================================
// Area left to the children of the ui container holding `object`: {x, y, w, h}
// in pixels, bottom-left origin in the container frame of height `frameHeight`.
// False when `object` is not held by a ui component.
NLSGRAPHICS_IMPEXP bool
uiParentContentRect(GraphicsObject* object, std::vector<double>& content, double& frameHeight);
//=============================================================================
bool
graphicsParentSizeInPixels(GraphicsObject* object, GOFigure* figure, double& width, double& height);
//=============================================================================
// Pixel size of the area holding a colorbar or a legend: its ui container content or its figure.
NLSGRAPHICS_IMPEXP void
decorationHostSizeInPixels(GraphicsObject* object, double& width, double& height);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
uiComponentParentSizeInPixels(GOUIComponent* component, double& width, double& height);
//=============================================================================
} // namespace Nelson
//=============================================================================
