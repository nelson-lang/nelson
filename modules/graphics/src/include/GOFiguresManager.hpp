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
#include "nlsGraphics_exports.h"
#include "GOWindow.hpp"
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
NLSGRAPHICS_IMPEXP std::map<int64, GOWindow*>
getFigureList();
//=============================================================================
NLSGRAPHICS_IMPEXP GOWindow*
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
NLSGRAPHICS_IMPEXP GOWindow*
getFigure(int64 id);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<int64>
getFigureGraphicsObjects();
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
}
//=============================================================================
