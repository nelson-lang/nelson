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
#include "NewFigure.hpp"
#include "GOWindow.hpp"
#include "GOWindowManager.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
GOFigure*
newFigure()
{
    uint64 id = getAvailableGOWindowID();
    return newFigure(id);
}
//=============================================================================
GOFigure*
newFigure(uint64 id)
{
    GOWindow* goWin = getGOWindow(id);
    if (goWin == nullptr) {
        goWin = new GOWindow(id);
        addGOWindow(id, goWin);
    }
    goWin->show();
    goWin->activateWindow();
    goWin->setWindowState(Qt::WindowNoState);
    return goWin->getGOFigure();
}
//=============================================================================
GOFigure*
newFigure(GOFigure* goFigure)
{
    if (goFigure == nullptr) {
        return newFigure();
    }
    return newFigure(goFigure->getParentWindow()->ID());
}
//=============================================================================
} // namespace Nelson
//=============================================================================
