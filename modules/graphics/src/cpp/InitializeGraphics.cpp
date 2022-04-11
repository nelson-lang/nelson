//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "InitializeGraphics.hpp"
#include "GraphicRoot.hpp"
#include "GOWindowManager.hpp"
#include "GraphicObjectDelete.hpp"
#include "NewFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
initializeGraphics()
{
    createGraphicRoot();
    return true;
}
//=============================================================================
bool
finishGraphics()
{
    std::vector<GOWindow*> goWindows = getGOWindows();
    for (auto& goWindow : goWindows) {
        GOFigure* goPtr = newFigure(goWindow->ID());
        graphicObjectDelete(goPtr);
    }
    goWindows.clear();
    return true;
}
//=============================================================================
}
//=============================================================================
