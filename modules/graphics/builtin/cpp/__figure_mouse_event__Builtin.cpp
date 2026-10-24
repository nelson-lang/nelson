//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "__figure_mouse_event__Builtin.hpp"
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "WebFigureBridgeExports.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// __figure_mouse_event__(f, action, [x y] [, target] [, selectionType]): test
// hook replaying a mouse event on figure f. action is 'press', 'motion' or
// 'release'; [x y] are pixels from the bottom-left corner of the drawing area.
// target is 'native' (default) or 'web' (the web client entry point).
// selectionType is 'normal' (default), 'extend', 'alt' or 'open'; it drives the
// gesture (modifiers / button / double click) exactly as a real pointer would.
// Returns true when the event was delivered.
ArrayOfVector
Nelson::GraphicsGateway::__figure_mouse_event__Builtin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 3, 5);
    nargoutcheck(nLhs, 0, 2);
    if (!argIn[0].isScalar() || (!argIn[0].isGraphicsObject() && !argIn[0].isNumeric())) {
        Error(L"Nelson:graphics:expectedGraphicsObject:expectedGraphicsObjectOrWebHandle",
            L"Expected graphics object or web graphics handle.");
    }
    const nelson_handle handle = argIn[0].isGraphicsObject()
        ? static_cast<const nelson_handle*>(argIn[0].dataPointer())[0]
        : static_cast<nelson_handle>(argIn[0].asDoubleScalar());
    if (isDeletedGraphicsObject(handle) || handle == HANDLE_ROOT_OBJECT
        || handle >= HANDLE_OFFSET_OBJECT) {
        Error(L"Nelson:graphics:expectedFigure:figureExpected", L"Figure object expected.");
    }
    std::wstring action = argIn[1].asWideString();
    ArrayOf position = argIn[2];
    position.promoteType(NLS_DOUBLE);
    if (position.elementCount() != 2) {
        Error(L"Nelson:graphics:expectedPoint", L"[x y] expected.");
    }
    const double* xy = static_cast<const double*>(position.dataPointer());
    bool viaWeb = argIn.size() > 3 && argIn[3].asWideString() == L"web";
    std::wstring selectionType = argIn.size() > 4 ? argIn[4].asWideString() : L"normal";
    // Gesture matching selectionType: 'extend' = shift-left, 'alt' = right
    // button, 'open' = left double click, 'normal' = plain left click.
    int button = 0;
    bool shiftDown = false;
    bool ctrlDown = false;
    bool doubleClick = false;
    if (selectionType == L"extend") {
        shiftDown = true;
    } else if (selectionType == L"alt") {
        button = 2;
    } else if (selectionType == L"open") {
        doubleClick = true;
    }
    bool delivered = false;
    bool listensToMotion = false;
    if (viaWeb) {
        // The mouse callbacks are queued, not run here: they execute at the
        // next drawnow()/pause(), never inside the statement of a running
        // script that drained this command (see __web_flush_callbacks__).
        delivered = nlsWebGuiFigureMouseEvent(static_cast<int64_t>(handle),
            wstring_to_utf8(action).c_str(), xy[0], xy[1], &listensToMotion, button, shiftDown,
            ctrlDown, doubleClick);
    } else {
        GOFigureHost* host = getFigure(handle);
        delivered = host != nullptr
            && host->injectHostMouseEvent(
                action, xy[0], xy[1], button, shiftDown, ctrlDown, doubleClick);
    }
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(delivered);
    if (nLhs > 1) {
        retval << ArrayOf::logicalScalar(listensToMotion);
    }
    return retval;
}
//=============================================================================
