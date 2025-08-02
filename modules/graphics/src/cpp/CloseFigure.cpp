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
closeCurrentFigure()
{
    int64 currentFigure = getCurrentFigure();
    if (currentFigure == NO_FIGURE) {
        return true;
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
        GOWindow* window = getFigure(num);
        if (!window) {
            return false;
        }
        GraphicsObject* goFigure = window->getGOFigure();
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
closeAllFigures()
{
    int64 currentFigureID = getCurrentFigure();
    bool closedOnce = false;
    while (currentFigureID != -1) {
        closeFigure(currentFigureID);
        currentFigureID = getCurrentFigure();
        closedOnce = true;
    }
    return closedOnce;
}
//=============================================================================
}
//=============================================================================
