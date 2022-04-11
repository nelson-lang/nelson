//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
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
    GOWindow* goWin = getGOWindow(id, true);
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
