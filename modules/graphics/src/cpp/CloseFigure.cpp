//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "CloseFigure.hpp"
#include "GraphicsEnvironment.hpp"
#include "GOHelpers.hpp"
#include "GOFiguresManager.hpp"
#include "GOFigure.hpp"
#include "GOHelpers.hpp"
#include "GraphicsObject.hpp"
#include "GOStringProperty.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
closeCurrentFigure(bool forceClose)
{
    int64 currentFigure = getCurrentFigure();
    if (currentFigure == NO_FIGURE) {
        return true;
    }
    if (forceClose) {
        return deleteFigure(currentFigure, true);
    }
    return closeFigure(currentFigure);
}
//=============================================================================
bool
closeFigureByID(go_handle ID)
{
    return false;
}
//=============================================================================
bool
closeFigureByName(const std::wstring& name)
{
    std::vector<int64> nums = getFigureGraphicsObjects();
    for (long long num : nums) {
        GOFigureHost* window = getFigure(num);
        if (!window) {
            return false;
        }
        GraphicsObject* goFigure = window->hostFigure();
        if (!goFigure) {
            return false;
        }
        GOStringProperty* propertyName
            = static_cast<GOStringProperty*>(goFigure->findProperty(GO_NAME_PROPERTY_NAME_STR));
        if (propertyName->data() == name) {
            return closeFigure(num);
        }
    }
    return false;
}
//=============================================================================
bool
closeFigureByName(const wstringVector& names)
{
    bool res = true;
    for (const auto& name : names) {
        res = closeFigureByName(name) && res;
    }
    return false;
}
//=============================================================================
bool
closeAllFigures(bool forceClose)
{
    std::vector<int64> figureIDs = getFigureGraphicsObjects();
    bool closedOnce = false;
    for (int64 currentFigureID : figureIDs) {
        if (getFigure(currentFigureID)) {
            bool closed
                = forceClose ? deleteFigure(currentFigureID, true) : closeFigure(currentFigureID);
            if (closed) {
                closedOnce = true;
            }
        }
        GraphicsEnvironment::flushGuiEvents(50);
    }
    GraphicsEnvironment::flushGuiEvents(50);
    return closedOnce;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
