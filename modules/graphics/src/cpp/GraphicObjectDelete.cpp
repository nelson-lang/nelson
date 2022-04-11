//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GraphicObjectDelete.hpp"
#include "GOWindowManager.hpp"
#include "GOFigure.hpp"
#include "GOWindow.hpp"
#include "GORoot.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
graphicObjectDelete(GraphicObject* goPtr)
{
    if (goPtr->isType(ROOT_TYPE_STR)) {
        return false;
    }
    if (goPtr->isType(FIGURE_TYPE_STR) && (goPtr->referenceCount() != 0)) {
        auto* goFigPtr = (GOFigure*)goPtr;
        GOWindow* goWinPtr = goFigPtr->getParentWindow();
        if (goWinPtr != nullptr) {
            removeGOWindow(goWinPtr->ID());
            goWinPtr->hide();
            delete goWinPtr;
        }
        goPtr->dereference();
        return true;
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
