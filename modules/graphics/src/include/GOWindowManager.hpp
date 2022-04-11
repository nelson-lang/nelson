//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <vector>
#include "nlsGraphics_exports.h"
#include "GOWindow.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSGRAPHICS_IMPEXP void
initializeGOWindowManager();
//=============================================================================
NLSGRAPHICS_IMPEXP void
finishGOWindowManager();
//=============================================================================
NLSGRAPHICS_IMPEXP uint64
getCurrentGOWindowID();
//=============================================================================
NLSGRAPHICS_IMPEXP uint64
findCurrentGOWindowID();
//=============================================================================
NLSGRAPHICS_IMPEXP uint64
getAvailableGOWindowID();
//=============================================================================
NLSGRAPHICS_IMPEXP bool
addGOWindow(uint64 id, GOWindow* goFig);
//=============================================================================
NLSGRAPHICS_IMPEXP bool
removeGOWindow(uint64 id);
//=============================================================================
NLSGRAPHICS_IMPEXP GOWindow*
getGOWindow(uint64 id, bool selectIt = false);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<GOWindow*>
getGOWindows();
//=============================================================================
} // namespace Nelson
//=============================================================================
