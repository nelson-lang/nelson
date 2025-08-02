//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <ctime>
#include <thread>
#include <chrono>
#include "waitforbuttonpressBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "GOFiguresManager.hpp"
#include "ProcessEvents.hpp"
#include "Error.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
GraphicsGateway::waitforbuttonpressBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);

    int64 currentFigureID = getCurrentFigure();
    if (currentFigureID == -1) {
        std::vector<int64> figs = getFigureGraphicsObjects();
        if (figs.empty()) {
            currentFigureID = createNewFigure();
        } else {
            currentFigureID = figs.back();
        }
    }
    GOWindow* win = getFigure(currentFigureID);
    if (win) {
        WAIT_PRESS_MODE waitPressMode = WAIT_PRESS_MODE::NONE;
        win->setModeWaitMouseOrKeyPressEvent(true);
        do {
            ProcessEvents();
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));

        } while (win->isWaitMouseOrKeyPressEvent(waitPressMode));
        if (waitPressMode == WAIT_PRESS_MODE::CLOSE) {
            Error(_W("waitforbuttonpress exit because figure has been deleted."),
                L"Nelson:UI:CancelWaitForKeyOrButtonPress");
        }
        retval << ArrayOf::doubleConstructor(waitPressMode == WAIT_PRESS_MODE::MOUSE ? 0. : 1.);
    } else {
        Error(_W("Cannot get current figure."));
    }
    return retval;
}
//=============================================================================
