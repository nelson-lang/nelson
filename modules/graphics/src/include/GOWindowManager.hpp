//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
getGOWindow(uint64 id);
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<GOWindow*>
getGOWindows();
//=============================================================================
} // namespace Nelson
//=============================================================================
