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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
#include "GOFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// SizeChangedFcn raised while a scope is alive runs when the outermost scope ends.
class NLSGRAPHICS_IMPEXP SizeChangedScope
{
public:
    SizeChangedScope();
    ~SizeChangedScope();
    SizeChangedScope(const SizeChangedScope&) = delete;
    SizeChangedScope&
    operator=(const SizeChangedScope&)
        = delete;
};
//=============================================================================
// While alive, what is raised waits for the next callback processing (drawnow, pause, prompt).
class NLSGRAPHICS_IMPEXP SizeChangedDeferral
{
public:
    SizeChangedDeferral();
    ~SizeChangedDeferral();
    SizeChangedDeferral(const SizeChangedDeferral&) = delete;
    SizeChangedDeferral&
    operator=(const SizeChangedDeferral&)
        = delete;
};
//=============================================================================
// Raises SizeChangedFcn of a figure or a container: at scope end, else queued.
NLSGRAPHICS_IMPEXP void
notifySizeChanged(GraphicsObject* go);
//=============================================================================
// Figure drawing area reported by the host; sizeChanged false for a repeated report.
NLSGRAPHICS_IMPEXP void
notifyFigureSizeReported(GOFigure* fig, bool sizeChanged);
//=============================================================================
// Raises what was held back while the figure was invisible.
NLSGRAPHICS_IMPEXP void
notifyFigureShown(GOFigure* fig);
//=============================================================================
// Raises what was held back while the container was invisible.
NLSGRAPHICS_IMPEXP void
notifyContainerShown(GraphicsObject* go);
//=============================================================================
// Page of a tab sized by its group: the first size is raised at once, a later one is deferred.
NLSGRAPHICS_IMPEXP void
notifyTabPageResized(GraphicsObject* tab, double width, double height);
//=============================================================================
// Size the Position of a uifigure tab still reports: its group resized it, the deferred run has
// not reached it yet. False when the tab reports its actual size.
NLSGRAPHICS_IMPEXP bool
reportedTabPageSize(GraphicsObject* tab, double& width, double& height);
//=============================================================================
// Raises what was deferred, once per object, in the order it was deferred.
NLSGRAPHICS_IMPEXP void
runDeferredSizeChanged();
//=============================================================================
// True when the Position origin of a uifigure container moved since the last call.
NLSGRAPHICS_IMPEXP bool
consumeContainerMove(GraphicsObject* go);
//=============================================================================
// Warns when a SizeChangedFcn was just set on an object that auto resizes its children.
NLSGRAPHICS_IMPEXP void
warnSizeChangedFcnDisabled(GraphicsObject* go);
//=============================================================================
// True when SizeChangedFcn is set and not disabled by AutoResizeChildren.
NLSGRAPHICS_IMPEXP bool
isSizeChangedFcnActive(GraphicsObject* go);
//=============================================================================
// Pixel size of a container without a widget: true when new or changed.
NLSGRAPHICS_IMPEXP bool
recordContainerPixelSize(GraphicsObject* go, double width, double height);
//=============================================================================
// Drops every record of an object: called when it is created or destroyed.
NLSGRAPHICS_IMPEXP void
forgetSizeChangedState(GraphicsObject* go);
//=============================================================================
} // namespace Nelson
//=============================================================================
