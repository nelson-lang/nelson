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
#include <map>
#include <string>
#include "nlsGraphics_exports.h"
#include "GOFigureHost.hpp"
#include "Types.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP void
initializeGraphic();
//=============================================================================
NLSGRAPHICS_IMPEXP void
shutdownGraphic();
//=============================================================================
NLSGRAPHICS_IMPEXP std::map<int64, GOFigureHost*>
getFigureList();
//=============================================================================
NLSGRAPHICS_IMPEXP GOFigureHost*
getHandleWindow(int64 fignum);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
findAvailableFigureId();
//=============================================================================
// deferLifecycleNotify leaves the "figure window opened" notification to the
// caller (pushNewFigureLifecycle), so a figure created with creation properties
// can apply its Position first and open its native window at the final size.
NLSGRAPHICS_IMPEXP int64
createNewFigure(bool show = true, bool deferLifecycleNotify = false);
//=============================================================================
NLSGRAPHICS_IMPEXP GOFigure*
getCurrentGOFigure();
//=============================================================================
// When outCreated is not null it reports whether a new figure was created (as
// opposed to selecting an existing one). deferLifecycleNotify behaves as in
// createNewFigure and only matters when a figure is created.
NLSGRAPHICS_IMPEXP int64
selectFigure(
    int64 fignum, bool show = true, bool deferLifecycleNotify = false, bool* outCreated = nullptr);
//=============================================================================
// Emit the deferred "figure window opened" notification for a figure created
// with deferLifecycleNotify = true, after its creation properties (notably
// Position) are applied, so the native window opens at the figure's final size
// instead of popping at the default size and then resizing.
NLSGRAPHICS_IMPEXP void
pushNewFigureLifecycle(int64 figNum);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
closeFigure(go_handle fignum, bool forceClose = true);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
deleteFigure(go_handle fignum, bool forceClose = true);
//=============================================================================
NLSGRAPHICS_IMPEXP GOFigureHost*
getFigure(int64 id);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<int64>
getFigureGraphicsObjects();
//=============================================================================
// Notify the desktop that a figure's WindowStyle changed at runtime so it
// can move between a separate native window and a docked panel. No-op unless a
// web figure window-style hook is registered (i.e. only in the desktop / GUI mode).
NLSGRAPHICS_IMPEXP void
notifyWebFigureWindowStyleChanged(GOFigure* fig);
//=============================================================================
// Notify the desktop that a figure's Visible changed at runtime so it can
// hide or show its separate native window. No-op unless a web figure visible
// hook is registered (i.e. only in the desktop / GUI mode).
NLSGRAPHICS_IMPEXP void
notifyWebFigureVisibleChanged(GOFigure* fig);
//=============================================================================
// The figure's display title, honoring NumberTitle and Name like the Qt
// desktop: "Figure N" / "Figure N: Name" (NumberTitle on) or "Name"
// (NumberTitle off). N is the 1-based figure number.
NLSGRAPHICS_IMPEXP std::wstring
computeFigureTitle(int64 figNum);
//=============================================================================
// Notify the desktop that a figure's title changed (Name / NumberTitle) so
// it can rename the panel and the separate native window.
NLSGRAPHICS_IMPEXP void
notifyWebFigureTitleChanged(GOFigure* fig);
//=============================================================================
// Notify the desktop that a figure's Position changed at runtime so it can
// slide the separate native window. No-op unless a web figure position hook is
// registered (i.e. only in the desktop / GUI mode).
NLSGRAPHICS_IMPEXP void
notifyWebFigurePositionChanged(GOFigure* fig);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
getCurrentFigure();
//=============================================================================
// current figure with visible handle (as gcf), a new figure when none
NLSGRAPHICS_IMPEXP int64
getOrCreateVisibleCurrentFigure();
//=============================================================================
NLSGRAPHICS_IMPEXP void
notifyCurrentFigureChanged(int64 figNum);
//=============================================================================
NLSGRAPHICS_IMPEXP void
saveFocus();
//=============================================================================
NLSGRAPHICS_IMPEXP void
restoreFocus();
//=============================================================================
// Inside this scope new figures use the headless host and open no window. Calls nest.
NLSGRAPHICS_IMPEXP void
beginHeadlessFigureScope();
//=============================================================================
NLSGRAPHICS_IMPEXP void
endHeadlessFigureScope();
//=============================================================================
NLSGRAPHICS_IMPEXP bool
isHeadlessFigureScopeActive();
//=============================================================================
} // namespace Nelson
//=============================================================================
