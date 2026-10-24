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
#include "GraphicsEnvironment.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
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

#if defined(__EMSCRIPTEN__)
    // The single-threaded WebAssembly worker cannot busy-wait for a key/mouse
    // event: spinning here blocks the worker, so the press could never be
    // delivered. Fail fast instead of hanging the browser.
    Error(L"Nelson:waitforbuttonpress:unsupportedInBrowser",
        _W("waitforbuttonpress is not available in the WebAssembly runtime."));
#endif

    int64 currentFigureID = getCurrentFigure();
    if (currentFigureID == -1) {
        std::vector<int64> figs = getFigureGraphicsObjects();
        if (figs.empty()) {
            currentFigureID = createNewFigure();
        } else {
            currentFigureID = figs.back();
        }
    }
    GOFigureHost* win = getFigure(currentFigureID);
    if (win) {
        WAIT_PRESS_MODE waitPressMode = WAIT_PRESS_MODE::NONE;
        win->setWaitMouseOrKeyPressEvent(true);
        do {
            GraphicsEnvironment::processGuiEvents();
            std::this_thread::sleep_for(std::chrono::milliseconds(uint64(1)));

        } while (win->isWaitingMouseOrKeyPressEvent(waitPressMode));
        if (waitPressMode == WAIT_PRESS_MODE::CLOSE) {
            Error(L"Nelson:UI:CancelWaitForKeyOrButtonPress",
                L"waitforbuttonpress exit because figure has been deleted.");
        }
        retval << ArrayOf::doubleScalar(waitPressMode == WAIT_PRESS_MODE::MOUSE ? 0. : 1.);
    } else {
        Error(L"Nelson:waitforbuttonpress:noCurrentFigure", L"Cannot get current figure.");
    }
    return retval;
}
//=============================================================================
