//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "GOFigureMouseEvents.hpp"
#include "GOFiguresManager.hpp"
#include "Types.hpp"
#include "WebFigureBridgeExports.hpp"
#include "characters_encoding.hpp"
//=============================================================================
extern "C"
{
    //=============================================================================
    NLSGRAPHICS_IMPEXP bool
    nlsWebGuiFigureMouseEvent(int64_t handle, const char* action, double x, double y,
        bool* listensToMotion, int button, bool shiftDown, bool ctrlDown, bool doubleClick)
    {
        Nelson::GOFigureHost* window = Nelson::getFigure(static_cast<Nelson::int64>(handle));
        if (window == nullptr || window->hostFigure() == nullptr || action == nullptr) {
            return false;
        }
        std::wstring selectionType
            = Nelson::resolveSelectionType(button, shiftDown, ctrlDown, doubleClick);
        bool motion = Nelson::dispatchFigureMouseEvent(
            window->hostFigure(), Nelson::utf8_to_wstring(action), x, y, selectionType);
        if (listensToMotion != nullptr) {
            *listensToMotion = motion;
        }
        return true;
    }
    //=============================================================================
}
//=============================================================================
