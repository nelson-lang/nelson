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
// The windowing layer registers how figure hosts are created; without a
// factory the manager cannot open figures (the Qt layer registers GOWindow).
NLSGRAPHICS_IMPEXP void
setFigureHostFactory(FigureHostFactory factory);
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
NLSGRAPHICS_IMPEXP int64
createNewFigure(bool show = true);
//=============================================================================
NLSGRAPHICS_IMPEXP GOFigure*
getCurrentGOFigure();
//=============================================================================
NLSGRAPHICS_IMPEXP int64
selectFigure(int64 fignum, bool show = true);
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
// Notify the web desktop that a figure's WindowStyle changed at runtime so it
// can move between a separate native window and a docked panel. No-op unless a
// web figure window-style hook is registered (i.e. only in WEB_GUI mode).
NLSGRAPHICS_IMPEXP void
notifyWebFigureWindowStyleChanged(GOFigure* fig);
//=============================================================================
// Notify the web desktop that a figure's Visible changed at runtime so it can
// hide or show its separate native window. No-op unless a web figure visible
// hook is registered (i.e. only in WEB_GUI mode).
NLSGRAPHICS_IMPEXP void
notifyWebFigureVisibleChanged(GOFigure* fig);
//=============================================================================
// The figure's display title, honoring NumberTitle and Name like the Qt
// desktop: "Figure N" / "Figure N: Name" (NumberTitle on) or "Name"
// (NumberTitle off). N is the 1-based figure number.
NLSGRAPHICS_IMPEXP std::wstring
computeFigureTitle(int64 figNum);
//=============================================================================
// Notify the web desktop that a figure's title changed (Name / NumberTitle) so
// it can rename the panel and the separate native window.
NLSGRAPHICS_IMPEXP void
notifyWebFigureTitleChanged(GOFigure* fig);
//=============================================================================
NLSGRAPHICS_IMPEXP int64
getCurrentFigure();
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
} // namespace Nelson
//=============================================================================
