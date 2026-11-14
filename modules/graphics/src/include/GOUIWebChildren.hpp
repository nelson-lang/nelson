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
//=============================================================================
namespace Nelson {
//=============================================================================
class GraphicsObject;
class GOFigure;
class GOUIComponent;
class RenderWeb;
//=============================================================================
// Describe the UI children (ui components and uicontrols) of a container to
// the web figure. `position` is the container frame in figure pixels: each
// child is placed in the container content area, then offset into it.
void
uiDescribeChildrenForWeb(
    GraphicsObject* container, RenderWeb& renderer, const std::vector<double>& position);
//=============================================================================
// True when the container has at least one graphics axes child. Such a panel's
// web overlay must be transparent: the axes (and the panel background) are drawn
// on the canvas underneath, so an opaque overlay div would hide them.
bool
uiContainerHasAxesChild(GraphicsObject* container);
//=============================================================================
// Widgetless container: raises SizeChangedFcn when its pixel size changed, nested ones first.
void
uiNotifyWebContainerGeometry(GOUIComponent* container);
//=============================================================================
// Same for the ui containers held by `parent` (a headless figure or a container).
void
uiNotifyWebChildContainers(GraphicsObject* parent);
//=============================================================================
// Same for every ui container of a figure; no-op unless its host is headless.
void
uiNotifyWebContainerSizes(GOFigure* figure);
//=============================================================================
} // namespace Nelson
//=============================================================================
